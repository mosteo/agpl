------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

--  The difference with Expres.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Conversions;
use  Agpl.Conversions;
with Agpl.Cr.Agent.Dummy;
with Agpl.Cr.Agent.Lists;
with Agpl.Cr.Assigner.Hungry3;
with Agpl.Cr.Plan_Assigner;
with Agpl.Cr.Plan_Assigner.Greedy1;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Htn.Plan_Node;
with Agpl.Htn.Plan.Utils;
with Agpl.Htn.Plan.Utils.Random;
with Agpl.Htn.Tasks.Lists;
with Agpl.Htn.Tasks.Maps;
with Agpl.Random;
with Agpl.Trace;   use Agpl.Trace;

with Ada.Numerics.Generic_Elementary_Functions;

package body Agpl.Cr.Mutable_Assignment is

   File : constant String := "[Mutable_Assignment] ";

   use type Htn.Tasks.Task_Id;
   use type Optimization.Cost;
   use type Optimization.Annealing.Probability;

   package Acm renames Agent_Cost_Maps;

   No_Task : Htn.Tasks.Task_Id renames Htn.Tasks.No_Task;

   function S is new
     Conversions.To_Str (Optimization.Annealing.Probability);

   function "<" (L, R : Minimax_Key) return Boolean is
      use Asu;
      use Optimization.Annealing;
   begin
      return
        L.Cost < R.Cost or else (L.Cost = R.Cost and then L.Agent < R.Agent);
   end "<";

   ---------------
   -- Add_Agent --
   ---------------

   procedure Add_Agent (This : in out Object; Name : in Agent_Id) is
   begin
      This.Context.Ref.all.Agents.Include (Name);
   end Add_Agent;

   ------------------
   -- Add_Mutation --
   ------------------

   procedure Add_Mutation (This    : in out Object;
                           Mutator : not null Mutation_Doer;
                           Undoer  : not null Mutation_Undoer;
                           Weight  : in     Float   := 1.0)
   is
   begin
      This.Context.Ref.all.Mutations.Append ((Doer   => Mutator,
                                              Undoer => Undoer,
                                              Weight => Weight,
                                              Prob   => 0.0));

      --  Adjust new probabilities
      declare
         Acum_Weight  : Float := 0.0;
         Total_Weight : Float := 0.0;
         M            : Mutation_Vectors.Object renames
           This.Context.Ref.Mutations;
      begin
         for I in M.Vector'Range loop
            Total_Weight := Total_Weight + M.Vector (I).Weight;
         end loop;
         for I in M.First .. M.Last loop
            Acum_Weight       := Acum_Weight + M.Vector (I).Weight;
            M.Vector (I).Prob := Optimization.Annealing.Probability
              (Acum_Weight / Total_Weight);
            if I = M.First then
               Log ("Mutation" & I'Img & ": P = " & S (M.Vector (I).Prob, 5),
                    Debug, Section => Log_Section);
            else
               Log ("Mutation" & I'Img & ": P = " &
                    S (M.Vector (I).Prob - M.Vector (I - 1).Prob, 5),
                    Debug, Section => Log_Section);
            end if;
         end loop;
      end;
   end Add_Mutation;

   ----------------
   -- Add_To_Bag --
   ----------------

   procedure Add_To_Bag (This    : in out Object;
                         Context : in Solution_Context_Access;
                         Bag     : in Bag_Key)
   is
      procedure Add (Key : in Bag_Key;
                     Bag : in out Solution_Context_Bags.Object)
      is
      begin
         pragma Assert (String (Key) = +Bag.Get_Context.Key);
         Bag.Append (Context);
         Context.Ref.all.Bag_Indexes.Insert (Key, Solution_Context_Bags.Last (Bag));
      end Add;
   begin
      Solution_Context_Bag_Maps.Update_Element
        (This.Bags, This.Bags.Find (Bag), Add'Access);
   end Add_To_Bag;

   ------------
   -- Adjust --
   ------------

   procedure Adjust     (This : in out Object) is
      pragma Unreferenced (This);
   begin
      null;
   end Adjust;

   ------------------------
   -- Clear_Dynamic_Part --
   ------------------------

   procedure Clear_Dynamic_Part (This : in out Object) is
   begin
      This.Contexts.Clear;
      This.Bags.Clear;
      This.Create_Empty_Bags;
   end Clear_Dynamic_Part;

   ----------------
   -- Clear_Undo --
   ----------------

   procedure Clear_Undo (This : in out Object) is
   begin
      This.Last_Mutation_Undo.Ass.Clear;
   end Clear_Undo;

   -----------------------
   -- Create_Empty_Bags --
   -----------------------

   procedure Create_Empty_Bags (This : in out Object) is
      Empty_Bag : Solution_Context_Bags.Object (First => 1);
   begin
      Empty_Bag.Set_Context ((Key => + String (All_Assigned_Tasks)));
      This.Bags.Insert (All_Assigned_Tasks, Empty_Bag);
   end Create_Empty_Bags;

   --------------------------
   -- Create_Some_Solution --
   --------------------------

   procedure Create_Some_Solution (This      : in out Object;
                                   Criterion : in Assignment_Criteria) is
      A : Cr.Assignment.Object;

      procedure Put_Agent (I : Agent_Sets.Cursor) is
         Ag : Cr.Agent.Dummy.Object;
      begin
         Ag.Set_Name (String (Agent_Sets.Element (I)));
         A.Set_Agent (Ag);
      end Put_Agent;
   begin
      A.Set_Valid;
      Agent_Sets.Iterate (This.Context.Ref.Agents, Put_Agent'Access);
      --  This ensures that all agents appear in the assignment, even if some
      --  haven't tasks.

      Set_Assignment (This, A, Criterion);
      --  Using an empty assignment we ensure that a greedy allocation will
      --  occur with all tasks in the plan.
   end Create_Some_Solution;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (This : in Task_Context) is
   begin
      Log ("Task Id:" & This.Job'Img, Always);
   end Debug_Dump;

   procedure Debug_Dump (This : in Or_Node_Context)
   is
   begin
      Log ("Selected branch:" & This.Current_Task'Img, Always);
   end Debug_Dump;

   -------------------------
   -- Debug_Dump_Contexts --
   -------------------------

   procedure Debug_Dump_Contexts (This : in Object) is
      procedure Debug_Dump_Context (I : Solution_Context_Maps.Cursor) is
         C : Solution_Context_Access renames Solution_Context_Maps.Element (I);
         procedure Debug_Dump_Attrs (I : Attribute_Maps.Cursor) is
         begin
            Log (Attribute_Maps.Key (I)'Img & " = " &
                 Attribute_Maps.Element (I), Always);
         end Debug_Dump_Attrs;
      begin
         Log ("Tag: " & External_Tag (C.Ref.all'Tag), Always);
         C.Ref.Debug_Dump;
         C.Ref.Attributes.Iterate (Debug_Dump_Attrs'Access);
      end Debug_Dump_Context;
   begin
      Log ("************ CONTEXTS DUMP FOLLOWS *****************", Always);
      This.Contexts.Iterate (Debug_Dump_Context'Access);
      Log ("************ CONTEXTS DUMP END     *****************", Always);
   end Debug_Dump_Contexts;

   --------------------
   -- Do_Heuristic_1 --
   --------------------

   procedure Do_Heuristic_1 (This : in out Object;
                             Desc :    out Ustring;
                             Undo :    out Undo_Info)
   is
      A : Cr.Assignment.Object := This.To_Assignment;
   begin
      Undo.Ass := A;
      Desc := +"Heuristic 1";
      declare
         use Cr.Assignment;
         New_Assignment : constant Cr.Assignment.Object :=
                            Cr.Assigner.Hungry3.Assign
                              ((Cr.Assigner.Object with Keep_Order => True),
                               Get_Agents_Without_Tasks (A),
                               Get_All_Tasks (A),
                               This.Context.Ref.Costs);
      begin
--         New_Assignment.Print_Assignment;

         if New_Assignment.Is_Valid then
            Set_Assignment (This, New_Assignment, This.Context.Ref.Criterion);
         else
            Log (File & "Hungry3 failed!", Warning);
         end if;
         --  Note: here Minimax will not be used since there are no new tasks.
      end;
   end Do_Heuristic_1;

   --------------------
   -- Do_Heuristic_2 --
   --------------------

   procedure Do_Heuristic_2 (This : in out Object;
                             Desc :    out Ustring;
                             Undo :    out Undo_Info)
   is
   begin
      Undo.Ass := This.To_Assignment;
      Desc     := +"Heuristic 2";
      declare
         use Cr.Assignment;
         New_Assignment : constant Cr.Assignment.Object :=
                            Plan_Assigner.Greedy1.Assign
                              ((Plan_Assigner.Object with null record),
                               Get_Agents_Without_Tasks (Undo.Ass),
                               This.Context.Ref.Plan,
                               This.Context.Ref.Costs,
                               This.Context.Ref.Criterion);
      begin
--         New_Assignment.Print_Assignment;

         if New_Assignment.Is_Valid then
            Set_Assignment (This, New_Assignment, This.Context.Ref.Criterion);
         else
            Log (File & "Plan_Assigner.Greedy1 failed!", Warning);
         end if;
         --  Note: here Minimax will not be used since there are no new tasks.
      end;
   exception
      when E : Constraint_Error =>
         Log (File & "Plan_Assigner.Greedy1 failed!", Warning);
         Log (Report (E), Warning);
   end Do_Heuristic_2;

   -----------------
   -- Do_Identity --
   -----------------

   procedure Do_Identity (This : in out Object;
                       Desc :    out Ustring;
                       Undo :    out Undo_Info)
   is
      pragma Unreferenced (This);
   begin
      Desc := +"Identity";
      Undo := (others => <>);
   end Do_Identity;

   --------------------
   -- Do_Remove_Task --
   --------------------

   procedure Do_Remove_Task (This : in out   Object;
                             Job  : not null Task_Context_Access)
   is
   begin
      --  Costs to be updated
      declare
         use Solution_Context_Maps;
         P, N       : Cursor;
         Prev, Next : Task_Context_Access;
      begin
         P := This.Contexts.Find (Task_Key (Job.Prev));
         N := This.Contexts.Find (Task_Key (Job.Next));
         if Has_Element (P) then
            Prev := Task_Context_Access (Element (P).Ref);
         end if;
         if Has_Element (N) then
            Next := Task_Context_Access (Element (N).Ref);
         end if;
         Update_Costs_Removing (This,
                                Prev,
                                Job,
                                Next);

         --  Adjust task chaining
         if Prev /= null then
            Prev.Next := Job.Next;
         end if;
         if Next /= null then
            Next.Prev := Job.Prev;
         end if;
         Job.Prev := No_Task;
         Job.Next := No_Task;
      end;

      declare
         Agent : constant Agent_Id :=
                   Agent_Id (Get_Attribute (Job.all'Access, Owner));
      begin
         pragma Assert (Acm.Element (This.Agent_Costs.Find (Agent)) =
                          Reevaluate_Agent_Cost (This, Agent));
         null;
      end;

      --  Remove from bags
      Remove_From_All_Bags (This, Job.all'Access);

      This.Contexts.Delete (Task_Key (Job.Job));
   end Do_Remove_Task;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs
   is
      Mm : constant Costs := This.Evaluate_Minimax;
      Ts : constant Costs := This.Evaluate_Totalsum;
   begin
      if Mm = Infinite or else Ts = Infinite then
         return Infinite;
      else
         return Mm * Costs (Criterion.Weight_Minimax) +
                Ts * Costs (Criterion.Weight_Totalsum);
   end Evaluate;

   ----------------------
   -- Evaluate_Minimax --
   ----------------------

   function Evaluate_Minimax (This : in Object) return Costs is
   begin
      if This.Valid then
         return This.Minimax.Last_Element.Cost;
      else
         return Infinite;
      end if;
   end Evaluate_Minimax;

   -----------------------
   -- Evaluate_Totalsum --
   -----------------------

   function Evaluate_Totalsum (This : in Object) return Costs is
   begin
      if This.Valid then
         return This.Totalsum;
      else
         return Infinite;
      end if;
   end Evaluate_Totalsum;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (This : in Solution_Context_Pointer;
                           Attr : in Solution_Context_Attributes) return String
   is
      use Attribute_Maps;
   begin
      return Element (Find (This.Attributes, Attr));
   end Get_Attribute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
   begin
      --  The static context
      This.Context.Bind (new Static_Context);

      --  The bags
      This.Create_Empty_Bags;
   end Initialize;

   -------------
   -- Is_Sane --
   -------------

   function Is_Sane (This : in Object) return Boolean is

      procedure Check_Contexts (I : Solution_Context_Maps.Cursor) is
         X : constant Solution_Context_Access :=
               Solution_Context_Maps.Element (I);
      begin
         if X.Ref.all in Task_Context then
            declare
               Node : constant Htn.Plan.Subplan :=
                        Htn.Plan.Get_Node
                          (This.Context.Ref.Plan, Task_Context (X.Ref.all).Job);
               use Htn.Plan_Node;
            begin
               if Get_Finished (Node) or else Get_Expanded (Node) then
                  raise Constraint_Error;
               end if;
            end;
         else
            raise Constraint_Error;
         end if;
      end Check_Contexts;

   begin
      Solution_Context_Maps.Iterate (This.Contexts, Check_Contexts'Access);

      return True;
   exception
      when E : others =>
         Log ("Checking solution sanity: " & Report (E), Error);
         return False;
   end Is_Sane;

   -------------------
   -- Last_Mutation --
   -------------------

   function Last_Mutation (This : in Object) return String is
   begin
      return +This.Last_Mutation_Description;
   end Last_Mutation;

   -----------------------------
   -- Moving_Solution_Context --
   -----------------------------

   procedure Moving_Solution_Context (Context : in out Solution_Context_Access;
                                      Bag     : in out Bag_Context;
                                      Prev,
                                      Curr    : in     Integer)
   is
      use Index_Maps;
   begin
      pragma Assert
        (Element (Context.Ref.Bag_Indexes.Find (Bag_Key (+Bag.Key))) = Prev);

      Context.Ref.all.Bag_Indexes.Delete (Bag_Key (+Bag.Key));
      Context.Ref.all.Bag_Indexes.Insert (Bag_Key (+Bag.Key), Curr);
   end Moving_Solution_Context;

   ------------
   -- Mutate --
   ------------

   procedure Mutate (This : in out Object) is
      pragma Assert (Is_Sane (This));
      use Optimization.Annealing;
      Luck : constant Probability := Probability (Random.Uniform);
      M    :          Mutation_Vectors.Object renames
        This.Context.Ref.Mutations;
   begin
      for I in M.First .. M.Last loop
         if Luck <= M.Vector (I).Prob then
            Log ("Performing mutation" & I'Img,
                 Debug, Section => Detail_Section);
            This.Last_Mutation_Index  := I;
            This.Last_Mutation_Exists := True;
            M.Vector (I).Doer (This,
                               This.Last_Mutation_Description,
                               This.Last_Mutation_Undo);
            return;
         end if;
      end loop;
      Log ("Mutate: No mutation performed!", Error);
      raise Program_Error with "No mutation performed";
   end Mutate;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (Old_Cost,
      New_Cost : in Optimization.Cost;
      Temp     : in Optimization.Annealing.Temperature)
      return        Optimization.Annealing.Acceptability
   is
      package Acceptability_Math is new
        Ada.Numerics.Generic_Elementary_Functions
          (Optimization.Annealing.Acceptability);

      use Optimization; use Annealing;
      use Acceptability_Math;
   begin
      if New_Cost < Old_Cost then
         return Acceptability'Last;
      else
         return
           (Acceptability (Old_Cost / New_Cost) *
              Acceptability (Temp)) ** 0.75; -- We increase the wildlity a bit.
      end if;
   exception
      when Constraint_Error =>
         Log ("Old_Cost: " & To_String (Float (Old_Cost)), Error);
         Log ("New_Cost: " & To_String (Float (New_Cost)), Error);
         Log ("Temp    : " & To_String (Float (Temp)), Error);
         raise;
   end Normalize;

   --------------------
   -- Reassign_Tasks --
   --------------------

   procedure Reassign_Tasks (This : in out Object; From, To : in Agent_Id) is
      procedure It (I : in Solution_Context_Maps.Cursor) is
         X : constant Solution_Context_Pointer :=
               Solution_Context_Maps.Element (I).Ref;
      begin
         if X.Attributes.Contains (Owner) then
            if Get_Attribute (X, Owner) = String (From) then
               Set_Attribute (X, Owner, String (To));
            end if;
         end if;
      end It;
   begin
      Solution_Context_Maps.Iterate (This.Contexts, It'Access);
   end Reassign_Tasks;

   ---------------------------
   -- Reevaluate_Agent_Cost --
   ---------------------------

   function Reevaluate_Agent_Cost (This  : in Object;
                                   Agent : in Agent_Id)
                                   return     Costs
   is
      C     : Cr.Cost_Matrix.Object renames This.Context.Ref.Costs;
      Total : Costs;

      use Solution_Context_Maps;

      --  Well use always the cost from prev to current
      procedure Add (I : Cursor) is
         X    : constant Solution_Context_Pointer := Element (I).Ref;
         Curr :          Costs;
      begin
         if X.all in Task_Context then
            if Agent_Id (Get_Attribute (X, Owner)) = Agent then
               declare
                  use Cost_Matrix;
                  use Htn.Tasks;
                  T : Task_Context renames Task_Context (X.all);
               begin
                  --  The case when Prev = No_Task is contemplated in the
                  --  Cost_Matrix object
                  Curr := Get_Cost (C, String (Agent), T.Prev, T.Job);
                  if Curr = Infinite then
                     Total := Infinite;
                  else
                     Total := Total + Curr;
                  end if;
               end;
            end if;
         end if;
      end Add;

      I : Cursor := This.Contexts.First;
   begin
      while Has_Element (I) loop
         Add (I);
         exit when Total = Infinite;
         Next (I);
      end loop;

      return Total;
   end Reevaluate_Agent_Cost;

   ----------------------
   -- Reevaluate_Costs --
   ----------------------

   procedure Reevaluate_Costs (This : in out Object) is
      procedure Ev (I : Agent_Sets.Cursor) is
         Id   : constant Agent_Id := Agent_Sets.Element (I);
         Cost : constant Costs    := Reevaluate_Agent_Cost (This, Id);
      begin
         This.Minimax.Insert ((Cost, +String (Id)));
         This.Agent_Costs.Insert (Id, Cost);
      end Ev;
   begin
      This.Totalsum := Reevaluate_Totalsum (This);

      This.Minimax.Clear;
      This.Agent_Costs.Clear;
      Agent_Sets.Iterate (This.Context.Ref.Agents, Ev'Access);

      This.Valid := This.Totalsum < Infinite;
   end Reevaluate_Costs;

   ------------------------
   -- Reevaluate_Minimax --
   ------------------------

   function Reevaluate_Minimax    (This : in Object)
                                   return    Costs
   is
      Minimax : Cost_Agent_Sets.Set;

      procedure Ev (I : Agent_Sets.Cursor) is
         Id   : constant Agent_Id := Agent_Sets.Element (I);
         Cost : constant Costs    := Reevaluate_Agent_Cost (This, Id);
      begin
         Minimax.Insert ((Cost, +String (Id)));
      end Ev;
   begin
      Agent_Sets.Iterate (This.Context.Ref.Agents, Ev'Access);
      return Minimax.Last_Element.Cost;
   end Reevaluate_Minimax;

   -------------------------
   -- Reevaluate_Totalsum --
   -------------------------

   function Reevaluate_Totalsum   (This : in Object)
                                   return    Costs
   is
      Total : Costs := 0.0;
      procedure Ev (I : Agent_Sets.Cursor) is
      begin
         Total := Total + Reevaluate_Agent_Cost (This, Agent_Sets.Element (I));
      end Ev;
   begin
      Agent_Sets.Iterate (This.Context.Ref.Agents, Ev'Access);
      return Total;
   end Reevaluate_Totalsum;

   ------------------
   -- Remove_Agent --
   ------------------
   --  O (n) or worse (depending on the heuristic used).
   procedure Remove_Agent (This : in out Object; Name : in Agent_Id) is
      Dummy_Desc : Ustring;
      Dummy_Undo : Undo_Info;
      C          : Static_Context_Access renames This.Context.Ref;
   begin
      C.Agents.Delete (Name);

      if This.Context.Ref.Agents.Is_Empty then
         Log ("Remove_Agent: No remaining agents!", Warning);
      else
         Reassign_Tasks (This, Name, Agent_Sets.First_Element (C.Agents));
         Do_Heuristic_1 (This, Dummy_Desc, Dummy_Undo);
      end if;
   end Remove_Agent;

   --------------------------
   -- Remove_From_All_Bags --
   --------------------------

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : in     Solution_Context_Pointer)
   is
      procedure Remove (Bag : in Solution_Context_Bag_Maps.Cursor) is
      begin
         Remove_From_Bag (This, Context, Bag);
      end Remove;
   begin
      Solution_Context_Bag_Maps.Iterate (This.Bags, Remove'Access);
   end Remove_From_All_Bags;

   ---------------------
   -- Remove_From_Bag --
   ---------------------

   procedure Remove_From_Bag (This    : in out Object;
                              Context : in     Solution_Context_Pointer;
                              Bag     : in     Bag_Key)
   is
   begin
      Remove_From_Bag (This, Context, This.Bags.Find (Bag));
   end Remove_From_Bag;

   ---------------------
   -- Remove_From_Bag --
   ---------------------

   procedure Remove_From_Bag
     (This    : in out Object;
      Context : in     Solution_Context_Pointer;
      Bag     : in     Solution_Context_Bag_Maps.Cursor)
   is
      procedure Remove (Key : in     Bag_Key;
                        Bag : in out Solution_Context_Bags.Object)
      is
         use Index_Maps;
      begin
         Solution_Context_Bags.Delete
           (Bag,
            Element (Find (Context.Bag_Indexes, Key)),
            Moving_Solution_Context'Access);
      end Remove;
   begin
      Solution_Context_Bag_Maps.Update_Element (This.Bags, Bag, Remove'Access);
   end Remove_From_Bag;

   --------------------
   -- Set_Assignment --
   --------------------

   procedure Set_Assignment (This      : in out Object;
                             Ass       : in     Cr.Assignment.Object;
                             Criterion : in Assignment_Criteria)
   is
      New_Ass       : Cr.Assignment.Object := Ass;
      Pending_Tasks : Htn.Tasks.Maps.Map;
      L             : constant Htn.Tasks.Lists.List :=
                        Htn.Plan.Enumerate_Tasks
                          (Htn.Plan.Utils.Random.Get_Any_Expansion
                             (Ass.Freeze_Plan (This.Context.Ref.Plan)),
                           Primitive => True,
                           Pending   => True);
      procedure Ins (I : Htn.Tasks.Lists.Cursor) is
         use Htn.Tasks.Lists;
      begin
         Pending_Tasks.Insert (Element (I).Get_Id, Element (I));
      end Ins;

      procedure Process_Agent (I : Cr.Agent.Lists.Cursor) is
         use Cr.Agent.Lists;
         use Htn.Tasks.Lists;
         A : constant Cr.Agent.Object'Class  := Element (I);
         T : constant Htn.Tasks.Lists.List   := A.Get_Tasks;
         J :          Htn.Tasks.Lists.Cursor := First (T);
      begin
         while Has_Element (J) loop
            declare
               C : constant Task_Context_Access := new Task_Context;
            begin
               C.Job := Element (J).Get_Id;
               if Has_Element (Previous (J)) then
                  C.Prev := Element (Previous (J)).Get_Id;
               end if;
               if Has_Element (Next (J)) then
                  C.Next := Element (Next (J)).Get_Id;
               end if;
               Set_Attribute (C.all'Access, Owner, Cr.Agent.Get_Name (A));

               declare
                  Cc : constant Solution_Context_Access :=
                         Context_Pointers.Bind (Solution_Context_Pointer (C));
               begin
                  This.Contexts.Insert (Task_Key (C.Job), Cc);
                  Add_To_Bag (This, Cc, All_Assigned_Tasks);
               end;
               Next (J);
            end;
         end loop;
      end Process_Agent;

      procedure Remove_Agent_Tasks (I : Cr.Agent.Lists.Cursor) is
         use Cr.Agent.Lists;
         use Htn.Tasks.Lists;
         A : constant Cr.Agent.Object'Class  := Element (I);
         T : constant Htn.Tasks.Lists.List   := A.Get_Tasks;
         J :          Htn.Tasks.Lists.Cursor := First (T);
      begin
         while Has_Element (J) loop
            Pending_Tasks.Delete (Element (J).Get_Id);
            Next (J);
         end loop;
      end Remove_Agent_Tasks;


   begin
      Clear_Dynamic_Part (This);

      --  Keep mapped tasks
      Htn.Tasks.Lists.Iterate (L, Ins'Access);

      --  Remove assigned tasks
      declare
         Agents : constant Cr.Agent.Lists.List := Ass.Get_Agents;
      begin
         Cr.Agent.Lists.Iterate (Agents, Remove_Agent_Tasks'Access);
      end;

      --  At this point, the Pending_Tasks lists contains only tasks in the plan
      --  that weren't in the assignment received. We are going to assign them
      --  in some greedy fashion.

      --  Do something with unassigned plan tasks
      while not Pending_Tasks.Is_Empty loop
         declare
            New_New_Ass : Cr.Assignment.Object;
            Success     : Boolean;
         begin
            Tasks.Insertions.Greedy
              (New_Ass,
               Htn.Tasks.Maps.First_Element (Pending_Tasks),
               This.Context.Ref.Costs,
               Criterion,
               New_New_Ass,
               Success);

            if not Success then
               Log ("Set_Assignment: cannot assign task " &
                    Pending_Tasks.First_Element.To_String, Error);
               raise Program_Error;
            end if;

            Pending_Tasks.Delete_First;
            New_Ass := New_New_Ass;
         end;
      end loop;

      --  At this point, we have inserted the new ones

      Log ("Set_Assignment: Unassigned tasks inserted",
           Debug, Section => Detail_Section);

      --  Create all contexts and things.
      declare
         Agents : constant Cr.Agent.Lists.List := New_Ass.Get_Agents;
      begin
         Cr.Agent.Lists.Iterate (Agents, Process_Agent'Access);
      end;

      Log ("Set_Assignment: Assigned tasks reinserted",
           Debug, Section => Detail_Section);

      Reevaluate_Costs (This);

      Log ("Set_Assignment: Costs reevaluated",
           Debug, Section => Detail_Section);
   end Set_Assignment;

   ---------------
   -- Set_Costs --
   ---------------

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Matrix.Object)
   is
      C : constant Static_Context_Access := This.Context.Ref;
   begin
      C.Costs := Costs;
      Reevaluate_Costs (This);
   end Set_Costs;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute (This : in Solution_Context_Pointer;
                            Attr : in Solution_Context_Attributes;
                            Val  : in String)
   is
      use Attribute_Maps;
   begin
      Include (This.Attributes, Attr, Val);
   end Set_Attribute;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object)
   is
      C : Static_Context_Access renames This.Context.Ref;
   begin
      Clear_Dynamic_Part (This);
      C.Plan := Htn.Plan.Inflate (Plan);
   end Set_Tasks;

   --------------
   -- Task_Key --
   --------------

   function Task_Key (Id : in Htn.Tasks.Task_Id) return Solution_Context_Key is
   begin
      return Solution_Context_Key ("task:" & Id'Img);
   end Task_Key;

   -------------------
   -- To_Assignment --
   -------------------

   function To_Assignment   (This : in Object) return Cr.Assignment.Object is
      Result : Cr.Assignment.Object;

      function Find_First (Agent : in Agent_Id) return Task_Context_Access is
         Curr : Task_Context_Access;
         use Solution_Context_Maps;
         I : Cursor := This.Contexts.First;
      begin
         --  Locate any:
         while Has_Element (I) loop
            if Get_Attribute (Element (I).Ref, Owner) = String (Agent) and then
              Element (I).Ref.all in Task_Context
            then
               Curr := Task_Context_Access (Element (I).Ref);
               exit;
            end if;
            Next (I);
         end loop;

         --  Go back to first one:
         if Curr /= null then
            while Curr.Prev /= No_Task loop
               Curr := Task_Context_Access
                 (Element (This.Contexts.Find (Task_Key (Curr.Prev))).Ref);
            end loop;
         end if;

         return Curr;
      end Find_First;

      procedure Assign_Agent (I : Agent_Sets.Cursor) is
         Agent_Name : constant Agent_Id            := Agent_Sets.Element (I);
         Agent      :          Cr.Agent.Dummy.Object;
         Curr       :          Task_Context_Access := Find_First (Agent_Name);
      begin
         Agent.Set_Name (String (Agent_Name));
         --  Add the empty agent so all appear in the assignment, even the ones
         --  without tasks:
         Assignment.Set_Agent (Result, Agent);
         while Curr /= null loop
            Assignment.Add
              (Result, Agent,
               Htn.Plan.Get_Task (This.Context.Ref.Plan, Curr.Job).all);
            exit when Curr.Next = No_Task;
            Curr := Task_Context_Access
              (Solution_Context_Maps.Element
                 (This.Contexts.Find (Task_Key (Curr.Next))).Ref);
         end loop;
      end Assign_Agent;
   begin
--      This.Debug_Dump_Contexts;
      Agent_Sets.Iterate (This.Context.Ref.Agents, Assign_Agent'Access);
      return Result;
   end To_Assignment;

   ----------
   -- Undo --
   ----------

   procedure Undo (This : in out Object) is
   begin
      if This.Last_Mutation_Exists then
         This.Context.Ref.Mutations.Vector
           (This.Last_Mutation_Index).Undoer (This, This.Last_Mutation_Undo);
         Clear_Undo (This);
         This.Last_Mutation_Exists := False;
      else
         raise Constraint_Error with "No mutation performed to be undone";
      end if;
   end Undo;

   -----------------------
   -- Undo_From_Scratch --
   -----------------------

   procedure Undo_From_Scratch (This : in out Object;
                                Undo : in     Undo_Info)
   is
   begin
      Log ("Undoing from scratch", Debug, Section => Detail_Section);
      Set_Assignment (This, Undo.Ass, Minimax);
   end Undo_From_Scratch;

   ----------------------
   -- Undo_Heuristic_1 --
   ----------------------

   procedure Undo_Heuristic_1 (This : in out Object; Undo : in  Undo_Info) is
   begin
      Undo_From_Scratch (This, Undo);
   end Undo_Heuristic_1;

   -------------------
   -- Undo_Identity --
   -------------------

   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info) is
      pragma Unreferenced (This, Undo);
   begin
      null;
   end Undo_Identity;

   ---------------------------
   -- Update_Costs_Removing --
   ---------------------------

   procedure Update_Costs_Removing
     (This               : in out Object;
      Prev_To_Be_Kept    : in     Task_Context_Access;
      Curr_To_Be_Deleted : in     Task_Context_Access;
      Next_To_Be_Kept    : in     Task_Context_Access)
   is
      Prev : Task_Context_Access renames Prev_To_Be_Kept;
      Next : Task_Context_Access renames Next_To_Be_Kept;
      Curr : Task_Context_Access renames Curr_To_Be_Deleted;

      Agent : constant Agent_Id :=
                Agent_Id (Get_Attribute (Curr.all'Access, Owner));
      Minus_1  : Costs := 0.0;
      Minus_2  : Costs := 0.0;
      Plus     : Costs := 0.0;
      Cost     : Costs := Acm.Element (This.Agent_Costs.Find (Agent));
      Cm       : Cost_Matrix.Object renames This.Context.Ref.Costs;

      Pr, Ne   : Htn.Tasks.Task_Id := No_Task;
   begin
      This.Agent_Costs.Delete (Agent);
      This.Minimax.Delete ((Cost, +String (Agent)));

      if Prev /= null then
         Pr := Prev.Job;
      end if;
      if Next /= null then
         Ne := Next.Job;
      end if;

      Minus_1 := Cost_Matrix.Get_Cost (Cm,
                                       String (Agent),
                                       Pr,
                                       Curr.Job);

      Minus_2 := Cost_Matrix.Get_Cost (Cm,
                                       String (Agent),
                                       Curr.Job,
                                       Ne);

      Plus    := Cost_Matrix.Get_Cost (Cm,
                                       String (Agent),
                                       Pr,
                                       Ne);

      Cost := Cost - Minus_1 - Minus_2 + Plus;

      This.Agent_Costs.Insert (Agent, Cost);
      This.Minimax.Insert ((Cost, +String (Agent)));
   end Update_Costs_Removing;

end Agpl.Cr.Mutable_Assignment;
