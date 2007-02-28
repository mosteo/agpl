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

--  The difference with Sancta.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Conversions;
use  Agpl.Conversions;
with Agpl.Cr.Assigner.Greedy_Minmax_Exhaustive;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Htn.Plan.Utils;
with Agpl.Htn.Plan.Utils.Random;
with Agpl.Htn.Plan_Node;
with Agpl.Htn.Tasks.Maps;
with Agpl.Random;
with Agpl.Trace;   use Agpl.Trace;

with Ada.Containers;
with Ada.Numerics.Generic_Elementary_Functions;

package body Agpl.Cr.Mutable_Assignment is

   Expensive_Checks : constant Boolean := False;

   File : constant String := "[Mutable_Assignment] ";

   use type Ada.Containers.Count_Type;
   use type Htn.Tasks.Task_Id;
   use type Optimization.Cost;
   use type Optimization.Annealing.Probability;

   package Acm renames Agent_Cost_Maps;

   function S is new Conversions.To_Str (Optimization.Annealing.Probability);
   function To_String is new Conversions.Fixed_To_Str (Cr.Costs);

   function "<" (L, R : Minimax_Key) return Boolean is
      use Asu;
      use Optimization.Annealing;
   begin
      return
        L.Cost < R.Cost or else (L.Cost = R.Cost and then L.Agent < R.Agent);
   end "<";

   function "+" (Cp : Solution_Context_Ptr) return Task_Context_Ptr;
   pragma Inline ("+");
   function "+" (Cp : Solution_Context_Ptr) return Task_Context_Ptr is
   begin
      return Task_Context_Ptr (Cp);
   end "+";

   ---------------
   -- Add_Agent --
   ---------------

   procedure Add_Agent (This : in out Object; A : in Cr.Agent.Object'Class) is
   begin
      This.Context.Ref.all.Agents.Include (A.Get_Name, A);
--      This.Set_Assignment (This.To_Assignment, This.Context.Ref.Criterion);
   end Add_Agent;

   ------------------
   -- Clear_Agents --
   ------------------

   procedure Clear_Agents (This : in out Object) is
   begin
      This.Context.Ref.all.Agents.Clear;
   end Clear_Agents;

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
         for I in M.First .. M.Last loop
            Total_Weight := Total_Weight + M.Vector (I).Weight;
         end loop;
         for I in M.First .. M.Last loop
            Acum_Weight       := Acum_Weight + M.Vector (I).Weight;
            Log ("Factors: " & Acum_Weight'Img & Total_Weight'Img, Never);
            M.Vector (I).Prob := Optimization.Annealing.Probability
              (Acum_Weight / Total_Weight);
            if I = M.First then
               Log ("Mutation" & I'Img & ": P = " & S (M.Vector (I).Prob, 5),
                    Debug, Section => Detail_Section);
            else
               Log ("Mutation" & I'Img & ": P = " &
                    S (M.Vector (I).Prob - M.Vector (I - 1).Prob, 5),
                    Debug, Section => Detail_Section);
            end if;
         end loop;
      end;
   end Add_Mutation;

   ---------------------
   -- Add_Or_Contexts --
   ---------------------

   procedure Add_Or_Contexts (This : in out Object;
                              Node : in     Htn.Plan.Subplan)
   is
      use Htn.Plan_Node;
      Parent    : constant Htn.Plan_Node.Node_Access := Get_Parent (Node);
   begin
      if Parent /= null then
         if Get_Kind (Parent) = Or_Node then
            if not This.Contexts.Contains (Or_Key (Get_Id (Parent))) then
               --  Check for duplicates, since we can arrive at an OR node
               --  from several tasks (children of an AND node underlying.
               declare
                  C : Or_Context := (Solution_Context with
                                     Node   => Parent,
                                     Branch => Node);
               begin
                  This.Add_To_Bag (C, All_Active_Or_Nodes);
                  This.Contexts.Insert (C.Key, C);
               end;
            else
               --  We may need to update the OR branch if it's the first time
               --  reaching it
               declare
                  Ctx_Ptr : constant Solution_Context_Ptr :=
                              This.Ptr (Or_Key (Get_Id (Parent)));
                  C       : Or_Context renames Or_Context (Ctx_Ptr.all);
               begin
                  if C.Branch /= Node then
                     C.Branch := Node;
                     Log ("Add_Or_Nodes: Corrected Or Branch taken.",
                          Debug, Detail_Section);
                  end if;
               end;
            end if;
         end if;
         This.Add_Or_Contexts (Parent);
      end if;
   end Add_Or_Contexts;

   ----------------
   -- Add_To_Bag --
   ----------------

   procedure Add_To_Bag (This    : in out Object;
                         Context : in out Solution_Context'Class;
                         Bag     : in Bag_Key)
   is
      procedure Add (Key : in Bag_Key;
                     Bag : in out Solution_Context_Bags.Object)
      is
      begin
         --  Log ("Adding " & S (Context.Key) & " to bag " & String (Key), Always);
         pragma Assert (String (Key) = +Bag.Get_Context.Key);
         Bag.Append (Context.Key);
         Context.Bag_Indexes.Insert (Key, Bag.Last);
      end Add;
   begin
      This.Bags.Update_Element (This.Bags.Find (Bag), Add'Access);
   end Add_To_Bag;

   -------------------
   -- Add_Undo_Move --
   -------------------

   procedure Add_Undo_Move (This : in     Object;
                            Job  : in     Task_Context_Ptr;
                            Undo : in out Undo_Internal)
   is
   begin
      Undo.Move_Stack.Append
        ((Moved_One  => Job.Job,
          Was_After  => Job.Prev,
          Was_Before => Job.Next,
          Owner_Was  => +Get_Attribute (Job, Owner),
          Minsum_Was => This.Minsum));
   end Add_Undo_Move;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Object) is
      procedure Do_It (Key : in     Bag_Key;
                       Bag : in out Solution_Context_Bags.Object) is
      begin
         Bag.Set_Context ((+String (Key), This'Unchecked_Access));
      end Do_It;
      procedure Do_Check (I : Solution_Context_Bag_Maps.Cursor) is
      begin
         This.Bags.Update_Element (I, Do_It'Access);
      end Do_Check;
   begin
      This.Bags.Iterate (Do_Check'Access);
   end Adjust;

   ----------------------------
   -- Adjust_Chain_Inserting --
   ----------------------------

   procedure Adjust_Chain_Inserting (This         : in out Object;
                                     After_This   : in     Task_Context_Ptr;
                                     Job          : in     Task_Context_Ptr;
                                     Before_This  : in     Task_Context_Ptr)
   is
      pragma Unreferenced (This);
      Aft : Task_Context_Ptr renames After_This;
      Bfr : Task_Context_Ptr renames Before_This;
   begin
      if (Aft /= null and then Bfr /= null and then Aft.Next /= Bfr.Job) or else
        (Aft /= null and then Bfr = null and then Aft.Next /= No_Task) or else
        (Bfr /= null and then Aft = null and then Bfr.Prev /= No_Task)
      then
         raise Constraint_Error with "Integrity violated!";
      end if;

      if Aft /= null then
         Aft.Next := Job.Job;
         Job.Prev := Aft.Job;
      else
         Job.Prev := No_Task;
      end if;

      if Bfr /= null then
         Bfr.Prev := Job.Job;
         Job.Next := Bfr.Job;
      else
         Job.Next := No_Task;
      end if;
   end Adjust_Chain_Inserting;

   ---------------------------
   -- Adjust_Chain_Removing --
   ---------------------------

   procedure Adjust_Chain_Removing (This : in out Object;
                                    Job  : in     Task_Context_Ptr) is
      Prev : constant Task_Context_Ptr := This.Get_Task_Context (Job.Prev);
      Next : constant Task_Context_Ptr := This.Get_Task_Context (Job.Next);
   begin
      --  Adjust task chaining
      if Prev /= null then
         Prev.Next := Job.Next;
      end if;
      if Next /= null then
         Next.Prev := Job.Prev;
      end if;
      Job.Prev := No_Task;
      Job.Next := No_Task;
   end Adjust_Chain_Removing;

   ---------------
   -- Agent_Key --
   ---------------

   function Agent_Key (Name : Agent_Id) return Solution_Context_Key is
   begin
      return Solution_Context_Key (+("A:" & Name));
   end Agent_Key;

   ---------------------
   -- Agent_Tasks_Bag --
   ---------------------

   function Agent_Tasks_Bag (Name : in Agent_Id) return Bag_Key is
   begin
      return Bag_Key ("A:" & Name);
   end Agent_Tasks_Bag;

   ----------------
   -- Bag_Length --
   ----------------

   function Bag_Length (This : in Object; Key : in Bag_Key) return Natural is
      Result : Natural;
      use Solution_Context_Bag_Maps;
      procedure Do_It (Key : Bag_Key; Bag : Solution_Context_Bags.Object) is
         pragma Unreferenced (Key);
      begin
         Result := Bag.Length;
      end Do_It;
   begin
      Query_Element (This.Bags.Find (Key), Do_It'Access);
      return Result;
   end Bag_Length;

   ------------------------
   -- Clear_Dynamic_Part --
   ------------------------

   procedure Clear_Dynamic_Part (This : in out Object) is
   begin
      This.Contexts.Clear;
      This.Bags.Clear;
      This.Create_Empty_Bags;
   end Clear_Dynamic_Part;

   ----------------------
   -- Create_Empty_Bag --
   ----------------------

   procedure Create_Empty_Bag (This : in out Object;
                               Key  : in     Bag_Key)
   is
      Empty_Bag : Solution_Context_Bags.Object (First => 1);
   begin
      Empty_Bag.Set_Context ((Key    => +String (Key),
                              Parent => This'Unchecked_Access));
      This.Bags.Insert (Key, Empty_Bag);
   end Create_Empty_Bag;

   -----------------------
   -- Create_Empty_Bags --
   -----------------------

   procedure Create_Empty_Bags (This : in out Object) is
   begin
      This.Create_Empty_Bag (All_Assigned_Tasks);
      This.Create_Empty_Bag (All_Agents);
      This.Create_Empty_Bag (All_Active_Or_Nodes);
   end Create_Empty_Bags;

   --------------------------
   -- Create_Some_Solution --
   --------------------------

   procedure Create_Some_Solution (This      : in out Object;
                                   Criterion : in Assignment_Criteria) is
      A : Cr.Assignment.Object;

      procedure Put_Agent (I : Agent_Maps.Cursor) is
      begin
         A.Set_Agent (Agent_Maps.Element (I));
      end Put_Agent;
   begin
      A.Set_Valid;
      This.Context.Ref.Agents.Iterate (Put_Agent'Access);
      --  This ensures that all agents appear in the assignment, even if some
      --  haven't tasks.

      Set_Assignment (This, A, Criterion);
      --  Using an empty assignment we ensure that a greedy allocation will
      --  occur with all tasks in the plan.
   end Create_Some_Solution;

   -----------------
   -- Common_Dump --
   -----------------

   procedure Common_Dump (This : in Solution_Context'Class) is
      procedure Dump_Attrs (I : Attribute_Maps.Cursor) is
      begin
         Log (Attribute_Maps.Key (I)'Img & " = " &
              Attribute_Maps.Element (I), Always);
      end Dump_Attrs;
      procedure Dump_Bags (I : Index_Maps.Cursor) is
         use Index_Maps;
      begin
         Log ("Bag idx for " & String (Key (I)) & " =" & Element (I)'Img,
              Always);
      end Dump_Bags;
   begin
      This.Attributes.Iterate (Dump_Attrs'Access);
      This.Bag_Indexes.Iterate (Dump_Bags'Access);
   end Common_Dump;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (This : in Task_Context) is
   begin
      Log ("Task Id:" & This.Job'Img & "; " &
           "Prev:" & This.Prev'Img & "; " &
           "Next:" & This.Next'Img,
           Always);
   end Debug_Dump;

   -------------------------
   -- Debug_Dump_Contexts --
   -------------------------

   procedure Debug_Dump_Contexts (This : in Object) is
      procedure Debug_Dump_Context (I : Solution_Context_Maps.Cursor) is
         C : constant Solution_Context'Class :=
               Solution_Context_Maps.Element (I);
      begin
         Log ("Tag: " & External_Tag (C'Tag), Always);
         C.Debug_Dump;
         Common_Dump (C);
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
                             Undo :    out Undo_Info)
   is
      U : Undo_Internal (From_Scratch);
      A : Cr.Assignment.Object := This.To_Assignment;
   begin
      U.Ass         := A;
      U.Description := +"Heuristic 1";
      Undo.Handle.Set (U);

      declare
         use Cr.Assignment;
         New_Assignment : constant Cr.Assignment.Object :=
                            Cr.Assigner.Greedy_MinMax_Exhaustive.Assign
                              ((Cr.Assigner.Object with Keep_Order => True),
                               Get_Agents_Without_Tasks (A),
                               Get_All_Tasks (A),
                               This.Context.Ref.Costs.Ref.all);
      begin
--         New_Assignment.Print_Assignment;

         if New_Assignment.Is_Valid then
            Set_Assignment (This, New_Assignment, This.Context.Ref.Criterion);
         else
            Log (File & "Assigner failed!", Warning);
         end if;
         --  Note: here criterion will not be used since there are no new tasks.
      end;
   end Do_Heuristic_1;

   -----------------
   -- Do_Identity --
   -----------------

   procedure Do_Identity (This : in out Object;
                          Undo :    out Undo_Info)
   is
      pragma Unreferenced (This);
      U : Undo_Internal := (Identity, +"IDENTITY");
   begin
      Log ("Identity mutation performed", Debug, Detail_Section);
      Undo.Handle.Set (U);
   end Do_Identity;

   --------------------
   -- Do_Insert_Task --
   --------------------

   procedure Do_Insert_Task (This        : in out Object;
                             After_This  : in Task_Context_Ptr;
                             Src         : in Task_Context'Class;
                             Before_This : in Task_Context_Ptr;
                             New_Owner   : in Agent_Id)
   is
      Src_Ptr : Task_Context_Ptr;
      Src_Cpy : Task_Context'Class := Src;
   begin
      Src_Cpy.Bag_Indexes.Clear;

      --  Insert context
      This.Contexts.Insert (Src_Cpy.Key, Src_Cpy);

      Src_Ptr := This.Ptr (Src_Cpy.Key);

      --  NO USE OF SRC/SRC_CPY AFTER THIS LINE  --

      declare
         Src, Src_Cpy : constant Natural := 0;
         pragma Unreferenced (Src, Src_Cpy);
         --  Dummy declaration to avoid Src use
      begin
         --  Set attributes
         Set_Attribute (Src_Ptr, Owner, New_Owner);
         --  Add to all assigned tasks bag
         This.Add_To_Bag (Src_Ptr.all, All_Assigned_Tasks);
         --  Add to agent task bag
         This.Add_To_Bag (Src_Ptr.all, Agent_Tasks_Bag (New_Owner));

         This.Update_Costs_Inserting
           (After_This,
            Src_Ptr,
            Before_This,
            New_Owner);

         This.Adjust_Chain_Inserting (After_This  => After_This,
                                      Job         => Src_Ptr,
                                      Before_This => Before_This);

         Log ("Inserting " & Src_Ptr.Job'Img &
              " after" & Src_Ptr.Prev'Img &
              " and before" & Src_Ptr.Next'Img &
              " owned by " & String (New_Owner),
                Debug, Detail_Section);

         declare
            Cost : Costs;
         begin
            if Expensive_Checks then
               Reevaluate_Agent_Cost (This, New_Owner, Cost);
               if Acm.Element (This.Agent_Costs.Find (New_Owner)) /= Cost then
                  Log ("Eval: " & To_String (Cost, 5) & " ?= " &
                       "Stor: " & To_String
                         (Acm.Element (This.Agent_Costs.Find (New_Owner)), 5),
                       Always);
                  raise Program_Error;
               end if;
            end if;
         end;
      end;
   end Do_Insert_Task;

   ------------------
   -- Do_Move_Task --
   ------------------

   procedure Do_Move_Task (This        : in out Object;
                           After_This  : in     Task_Context_Ptr;
                           Src         : in out Task_Context_Ptr;
                           Before_This : in     Task_Context_Ptr;
                           New_Owner   : in     Agent_Id)
   is
      Src_Copy : Task_Context := Task_Context (Src.all);
   begin
      This.Do_Remove_Task (Src);
      Do_Insert_Task (This,
                      After_This,
                      Src_Copy,
                      Before_This,
                      New_Owner);
      Src := This.Ptr (Src_Copy.Key);
   end Do_Move_Task;

   ----------------------
   -- Descend_Removing --
   ----------------------

   procedure Descend_Removing (This : in out Object;
                               Node : in     Htn.Plan.Subplan;
                               Undo : in out Undo_Internal) is
      use Htn.Plan_Node;
   begin
      Log ("Descending into " & Get_Kind (Node)'Img & " " & Get_Id (Node),
           Debug, Detail_Section);
      case Get_Kind (Node) is
         when Task_Node =>
            if Get_Expanded (Node) then
               --  Compound, do nothing and go down:
               This.Descend_Removing (Get_Expansion (Node), Undo);
            else
               --  Remove the task + context
               Log ("Removing its primitive task " &
                    Get_Task (Node).all.To_String,
                    Debug, Detail_Section);
               declare
                  Tc  : Task_Context_Ptr :=
                          This.Get_Task_Context (Get_Task (Node).all.Get_Id);
               begin
                  Undo.Or_Stack.Append ((Was_Before => Tc.Next,
                                         Moved_One  => Tc.Job,
                                         Was_After  => Tc.Prev,
                                         Owner_Was  => +Get_Attribute (Tc, Owner),
                                         Minsum_Was => This.Minsum));
                  This.Do_Remove_Task (Tc);
               end;
            end if;
         when And_Node =>
            declare
               Children : constant Node_Vectors.Vector :=
                            Get_Children (Node);
            begin
               for I in Children.First_Index .. Children.Last_Index loop
                  This.Descend_Removing (Children.Element (I), Undo);
               end loop;
            end;
         when Or_Node =>
            --  We must remove its context and go down its active branch
            declare
               Branch_Ptr : constant Solution_Context_Ptr :=
                              This.Ptr (Or_Key (Get_Id (Node)));
            begin
               This.Descend_Removing (Or_Context (Branch_Ptr.all).Branch, Undo);
               This.Remove_Context (Or_Key (Get_Id (Node)));
            end;
      end case;
   end Descend_Removing;

   --------------------
   -- Do_Remove_Task --
   --------------------

   procedure Do_Remove_Task (This : in out Object;
                             Job  : in out Task_Context_Ptr)
   is
      Agent : constant Agent_Id := Agent_Id (Get_Attribute (Job, Owner));
   begin
      Log ("Removing " & Job.Job'Img &
           " after" & Job.Prev'Img &
           " and before" & Job.Next'Img &
           " owned by " & Get_Attribute (Job, Owner),
           Debug, Detail_Section);

      --  Costs to be updated
      declare
         use Solution_Context_Maps;
         Prev, Next : Task_Context_Ptr;
      begin
         Prev := This.Ptr (Task_Key (Job.Prev));
         Next := This.Ptr (Task_Key (Job.Next));
         Update_Costs_Removing (This,
                                Prev_To_Be_Kept    => Prev,
                                Curr_To_Be_Deleted => Job,
                                Next_To_Be_Kept    => Next,
                                Former_Owner       => Get_Attribute
                                  (Job.all'Access, Owner));

         This.Adjust_Chain_Removing (Job);
      end;

      --  Remove from earth surface
      This.Remove_Context (Task_Key (Job.Job));

      Job := null;

      declare
         Cost  : Costs;
      begin
         if Expensive_Checks then
            Reevaluate_Agent_Cost (This, Agent, Cost);
            if Acm.Element (This.Agent_Costs.Find (Agent)) /= Cost then
               Log ("Eval: " & Cost'Img & " ?= " &
                    "Stor: " &
                      Acm.Element (This.Agent_Costs.Find (Agent))'Img,
                    Always);
               This.Debug_Dump_Contexts;
               raise Program_Error;
            end if;
         end if;
      end;
   end Do_Remove_Task;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This      : in Object) return Costs is
   begin
      return This.Evaluate (This.Context.Ref.Criterion);
   end Evaluate;

   function Evaluate (This      : in Object) return Optimization.Cost is
      C : constant Cr.Costs := This.Evaluate;
   begin
      if C = Cr.Infinite then
         return Optimization.Infinite;
      else
         return Optimization.Cost (C);
      end if;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs
   is
   begin
      return Evaluate (Criterion,
                       Minmax => This.Evaluate_Minimax,
                       Minsum => This.Evaluate_Totalsum);
   end Evaluate;

   ----------------------
   -- Evaluate_Minimax --
   ----------------------

   function Evaluate_Minimax (This : in Object) return Costs is
   begin
      if This.Valid then
         if This.Minmax.Is_Empty then
            return 0.0;
         else
            return This.Minmax.Last_Element.Cost;
         end if;
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
         return This.MinSum;
      else
         return Infinite;
      end if;
   end Evaluate_Totalsum;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (Context : not null access Solution_Context'Class;
                           Attr    : in Solution_Context_Attributes)
                           return       String
   is
   begin
      return Get_Attribute (Context.all, Attr);
   end Get_Attribute;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (Context : in Solution_Context'Class;
                           Attr    : in Solution_Context_Attributes)
                           return String
   is
      use Attribute_Maps;
   begin
      return Element (Context.Attributes.Find (Attr));
   end Get_Attribute;

   ----------------------
   -- Get_Task_Context --
   ----------------------

   function Get_Task_Context (This : in Object;
                              Id   : in Htn.Tasks.Task_Id)
                              return    Task_Context_Ptr
   is
      use Solution_Context_Maps;
      use Htn.Tasks;
   begin
      if Id = No_Task then
         return null;
      else
         return This.Ptr (Task_Key (Id));
      end if;
   end Get_Task_Context;

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

      procedure Check_Or_Parents (Node : in Htn.Plan.Subplan) is
         use Htn.Plan_Node;
      begin
         if Get_Kind (Node) = Or_Node and then
           not This.Contexts.Contains (Or_Key (Get_Id (Node)))
         then
            This.Context.Ref.Plan.Print_Tree_Summary;
            This.Debug_Dump_Contexts;
            raise Program_Error with "Missing OR ancestor for some task!";
         end if;
      end Check_Or_Parents;

      procedure Check_Contexts (I : Solution_Context_Maps.Cursor) is
         X : constant Solution_Context'Class :=
               Solution_Context_Maps.Element (I);
      begin
         if X in Task_Context then
            declare
               Node : constant Htn.Plan.Subplan :=
                        Htn.Plan.Get_Node
                          (This.Context.Ref.Plan,
                           Task_Context (X).Job);
               use Htn.Plan_Node;
            begin
               if Get_Finished (Node) or else Get_Expanded (Node) then
                  raise Constraint_Error
                    with "Compound or finished task is assigned";
               else
                 Check_Or_Parents (Node);
               end if;
            end;
         elsif X in Agent_Context then
            null;
         elsif X in Or_Context then
            --  Check the chosen branch is there
            declare
               use Htn.Plan_Node;
               Oc : Or_Context renames Or_Context (X);
            begin
               if Get_Kind (Oc.Branch) = Task_Node and then
                 not Get_Expanded (Oc.Branch)
               then
                  if not This.Contexts.Contains
                    (Task_Key (Get_Task (Oc.Branch).all.Get_Id))
                  then
                     Log ("Missing task context for task " &
                          Get_Task (Oc.Branch).all.Get_Id'Img &
                          "; branch taken of " & Get_Id (Oc.Node),
                          Always, Log_Section);
                     raise Program_Error with "Missing task for OR branch";
                  end if;
               end if;
            end;
         else
            raise Constraint_Error
              with "Unexpected context kind: " & External_Tag (X'Tag);
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

   ---------
   -- Key --
   ---------

   function Key (This : in Task_Context) return Solution_Context_Key is
   begin
      return Task_Key (This.Job);
   end Key;

   -------------------
   -- Last_Mutation --
   -------------------

   function Last_Mutation (This : in Object) return String is
   begin
      return +This.Last_Mutation;
   end Last_Mutation;

   -----------------------------
   -- Moving_Solution_Context --
   -----------------------------

   procedure Moving_Solution_Context (Context : in out Solution_Context_Key;
                                      Bag     : in out Bag_Context;
                                      Prev,
                                      Curr    : in     Integer)
   is
      use Index_Maps;
      C : constant Solution_Context_Ptr := Bag.Parent.Ptr (Context);
   begin
      pragma Assert
        (Element
           (C.Bag_Indexes.Find (Bag_Key (+Bag.Key))) = Prev);

      C.Bag_Indexes.Delete (Bag_Key (+Bag.Key));
      C.Bag_Indexes.Insert (Bag_Key (+Bag.Key), Curr);
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
      --  This.Debug_Dump_Contexts;
      --  Log ("** Minsum is " & To_String (This.Minsum), Always);

      if not This.Valid then
         Log ("Attempt to mutate an invalid solution!", Error, Log_Section);
         raise Constraint_Error with "Attempt to mutate an invalid solution!";
      end if;

      if This.Context.Ref.Agents.Is_Empty or else
        This.Context.Ref.Plan.Is_Empty
      then
         Log ("Empty plan, mutating to identity", Debug, Detail_Section);
         This.Was_Valid := This.Valid;
         This.Undoer    := Undo_Identity'Access;
         This.Do_Identity (This.Undo);
         This.Last_Mutation := This.Undo.Handle.Ref.Description;
         return;
      end if;

      ------------------------------------

      for I in M.First .. M.Last loop
         if Luck <= M.Vector (I).Prob then
            This.Was_Valid := This.Valid;

            Log ("Performing mutation" & I'Img & "; Valid: " & This.Valid'Img,
                 Debug, Section => Detail_Section);

            This.Undoer := M.Vector (I).Undoer;
            M.Vector (I).Doer (This, This.Undo);
            This.Last_Mutation := This.Undo.Handle.Ref.Description;

            Log ("Mutated: " & (+This.Undo.Handle.Get.Description) &
                 "; Valid: " & This.Valid'Img,
                 Debug, Section => Detail_Section);

            if Expensive_Checks then
               declare
                  Old_Cost : constant Costs :=
                               This.Evaluate (This.Context.Ref.Criterion);
                  Valid    : constant Boolean := This.Valid;
               begin
                  This.Reevaluate_Costs;
                  if Old_Cost /= This.Evaluate (This.Context.Ref.Criterion) then
                     Log ("Manual cost (" & Valid'Img & "):" & Old_Cost'Img &
                          " should equal reeval" &
                          This.Evaluate (This.Context.Ref.Criterion)'Img &
                          " (Valid: " & This.Valid'Img & ")",
                          Always, Log_Section);
                     raise Program_Error with "Mismatch after mutation";
                  end if;
               end;
            end if;
            return;
         end if;
      end loop;

      Log ("Mutate: No mutation performed!" & Luck'Img, Error, Log_Section);
      for I in M.First .. M.Last loop
         Log ("Prob. for mut." & I'Img & ":" & M.Vector (I).Prob'Img,
              Error, Log_Section);
      end loop;
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
      elsif New_Cost = 0.0 then
         return Acceptability'First;
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

   -----------------
   -- No_Task_Key --
   -----------------

   function No_Task_Key return Task_Context_Key is
   begin
      return Task_Context_Key (Task_Key (No_Task));
   end No_Task_Key;

   ------------------------
   -- Num_Assigned_Tasks --
   ------------------------

   function Num_Assigned_Tasks (This : in Object) return Natural is
   begin
      return This.Bag_Length (All_Assigned_Tasks);
   end Num_Assigned_Tasks;

   ---------
   -- Ptr --
   ---------

   function Ptr (This : in Object;
                 Key  : in Task_Context_Key) return Task_Context_Ptr
   is
      Result : Task_Context_Ptr;
      procedure Do_It (Key : Solution_Context_Key; C : Solution_Context'Class)
      is
         pragma Unreferenced (Key);
      begin
         pragma Unrestricted_Access;
         Result := Task_Context (C)'Unrestricted_Access;
      end Do_It;
   begin
      if Key = No_Task_Key then
         return null;
      else
         Solution_Context_Maps.Query_Element
           (This.Contexts.Find (Solution_Context_Key (Key)), Do_It'Access);
         return Result;
      end if;
   end Ptr;
   --  Gigantic ugly hack probably will blow out everything

   ---------
   -- Ptr --
   ---------

   function Ptr (This : in Object;
                 Key  : in Solution_Context_Key) return Solution_Context_Ptr
   is
      Result : Solution_Context_Ptr;
      procedure Do_It (Key : Solution_Context_Key; C : Solution_Context'Class)
      is
         pragma Unreferenced (Key);
      begin
         pragma Unrestricted_Access;
         Result := C'Unrestricted_Access;
      end Do_It;
   begin
      Solution_Context_Maps.Query_Element
        (This.Contexts.Find (Key), Do_It'Access);
      return Result;
   end Ptr;

   ---------
   -- Ptr --
   ---------

   function Ptr (This : in Object;
                 Key  : in Solution_Context_Key) return Task_Context_Ptr
   is
      Result : Task_Context_Ptr;
      procedure Do_It (Key : Solution_Context_Key; C : Solution_Context'Class)
      is
         pragma Unreferenced (Key);
      begin
         pragma Unrestricted_Access;
         Result := Task_Context (C)'Unrestricted_Access;
      end Do_It;
   begin
      if Task_Context_Key (Key) = No_Task_Key then
         return null;
      else
         Solution_Context_Maps.Query_Element
           (This.Contexts.Find (Key), Do_It'Access);
         return Result;
      end if;
   end Ptr;

   --------------------
   -- Reassign_Tasks --
   --------------------

   procedure Reassign_Tasks (This : in out Object; From, To : in Agent_Id) is
      procedure Do_It (I : in Solution_Context_Maps.Cursor) is
         procedure Do_It (Key : in     Solution_Context_Key;
                          X   : in out Solution_Context'Class)
         is
            pragma Unreferenced (Key);
         begin
            if X.Attributes.Contains (Owner) then
               if Get_Attribute (X'Access, Owner) = String (From) then
                  Set_Attribute (X'Access, Owner, String (To));
               end if;
            end if;
         end Do_It;
      begin
         This.Contexts.Update_Element (I, Do_It'Access);
      end Do_It;
   begin
      This.Contexts.Iterate (Do_It'Access);
   end Reassign_Tasks;

   ---------------------------
   -- Reevaluate_Agent_Cost --
   ---------------------------

   procedure Reevaluate_Agent_Cost (This  : in out Object;
                                    Agent : in     Agent_Id;
                                    Cost  :    out Costs)
   is
      C     : Cr.Cost_Cache.Object'Class renames This.Context.Ref.Costs.Ref.all;
      Total : Costs := 0.0;

      use Solution_Context_Maps;

      --  Well use always the cost from prev to current
      procedure Do_It (I : Cursor) is
         procedure Do_It (Key : in Solution_Context_Key;
                          X   : in Solution_Context'Class)
         is
            pragma Unreferenced (Key);
            Curr :          Costs;
         begin
            if X in Task_Context then
               if Agent_Id (Get_Attribute (X, Owner)) = Agent then
                  declare
                     use Cost_Cache;
                     use Htn.Tasks;
                     T : Task_Context renames Task_Context (X);
                  begin
                     --  The case when Prev = No_Task is contemplated in the
                     --  Cost_Cache object
                     Curr := Get_Cost (C, String (Agent), T.Prev, T.Job);
                     if Curr = Infinite then
                        Curr := Cost_For_Invalid_Task;
                        This.Valid := False;
                     end if;

                     Total := Total + Curr;
                  end;
               end if;
            end if;
         end Do_It;
      begin
         Solution_Context_Maps.Query_Element (I, Do_It'Access);
      end Do_It;

   begin
      This.Contexts.Iterate (Do_It'Access);
      Cost := Total;
   end Reevaluate_Agent_Cost;

   ----------------------
   -- Reevaluate_Costs --
   ----------------------

   procedure Reevaluate_Costs (This : in out Object) is
      procedure Ev (I : Agent_Maps.Cursor) is
         Id   : constant Agent_Id := Agent_Maps.Key (I);
         Cost : Costs;
      begin
         Reevaluate_Agent_Cost (This, Id, Cost);
         This.MinMax.Insert ((Cost, +String (Id)));
         This.Agent_Costs.Insert (Id, Cost);
      end Ev;
   begin
      --  Don't touch the Valid flag. It could be valid or not at this point.

      Reevaluate_Minsum (This, This.Minsum);

      This.MinMax.Clear;
      This.Agent_Costs.Clear;
      Agent_Maps.Iterate (This.Context.Ref.Agents, Ev'Access);
   end Reevaluate_Costs;

   ------------------------
   -- Reevaluate_Minimax --
   ------------------------

   procedure Reevaluate_Minmax (This : in out Object;
                                Cost :    out Costs)
   is
      Minimax : Cost_Agent_Sets.Set;

      procedure Ev (I : Agent_Maps.Cursor) is
         Id   : constant Agent_Id := Agent_Maps.Key (I);
      begin
         This.Reevaluate_Agent_Cost (Id, Cost);
         Minimax.Insert ((Cost, +String (Id)));
      end Ev;
   begin
      Agent_Maps.Iterate (This.Context.Ref.Agents, Ev'Access);
      Cost := Minimax.Last_Element.Cost;
   end Reevaluate_Minmax;

   -------------------------
   -- Reevaluate_Totalsum --
   -------------------------

   procedure Reevaluate_Minsum (This : in out Object;
                                Cost :    out Costs)
   is
      procedure Ev (I : Agent_Maps.Cursor) is
         Local : Costs;
      begin
         This.Reevaluate_Agent_Cost (Agent_Maps.Element (I).Get_Name, Local);
         Cost := Cost + Local;
      end Ev;
   begin
      Cost := 0.0;
      Agent_Maps.Iterate (This.Context.Ref.Agents, Ev'Access);
   end Reevaluate_Minsum;

   ------------------
   -- Remove_Agent --
   ------------------
   --  O (n) or worse (depending on the heuristic used).
   procedure Remove_Agent (This : in out Object; Name : in Agent_Id) is
      Dummy_Undo : Undo_Info;
      C          : Static_Context_Access renames This.Context.Ref;
   begin
      C.Agents.Delete (Name);

      if This.Context.Ref.Agents.Is_Empty then
         Log ("Remove_Agent: No remaining agents!", Warning);
      else
         Reassign_Tasks
           (This, Name, Agent_Maps.First_Element (C.Agents).Get_Name);
         This.Do_Heuristic_1 (Dummy_Undo);
      end if;
   end Remove_Agent;

   --------------------
   -- Remove_Context --
   --------------------

   procedure Remove_Context (This : in out Object;
                             Key  : in     Solution_Context_Key)
   is
      Ptr : constant Solution_Context_Ptr := This.Ptr (Key);
   begin
      This.Remove_From_All_Bags (Ptr);
      This.Contexts.Delete (Key);
   end Remove_Context;

   --------------------------
   -- Remove_From_All_Bags --
   --------------------------

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : access Solution_Context'Class)
   is
      Context_Indexes : Ustring_Vector.Object (First => 1);

      procedure Do_It (Bag : in Index_Maps.Cursor) is
      begin
         Context_Indexes.Append (+String (Index_Maps.Key (Bag)));
      end Do_It;
   begin
      Context.Bag_Indexes.Iterate (Do_It'Access);
      for I in Context_Indexes.First .. Context_Indexes.Last loop
         This.Remove_From_Bag
           (Context.all, Bag_Key (+Context_Indexes.Vector (I)));
      end loop;
   end Remove_From_All_Bags;

   ---------------------
   -- Remove_From_Bag --
   ---------------------

   procedure Remove_From_Bag (This    : in out Object;
                              Context : in out Solution_Context'Class;
                              Bag     : in     Bag_Key)
   is
   begin
      This.Remove_From_Bag (Context, This.Bags.Find (Bag));
   end Remove_From_Bag;

   ---------------------
   -- Remove_From_Bag --
   ---------------------

   procedure Remove_From_Bag
     (This    : in out Object;
      Context : in out Solution_Context'Class;
      Bag     : in     Solution_Context_Bag_Maps.Cursor)
   is
      procedure Do_It (Key : in     Bag_Key;
                       Bag : in out Solution_Context_Bags.Object)
      is
         use Index_Maps;
      begin
--           Log ("Remove from bag: Ctx: " & (+Ustring (Context.Key)) &
--                "; Bag: " & String (Key), Always, Detail_Section);
--           Log ("Bag index:" & Element (Find (Context.Bag_Indexes, Key))'Img,
--                Always, Detail_Section);

         Solution_Context_Bags.Delete
           (Bag,
            Element (Find (Context.Bag_Indexes, Key)),
            Moving_Solution_Context'Access);

         Context.Bag_Indexes.Delete (Key);
      end Do_It;
   begin
      This.Bags.Update_Element (Bag, Do_It'Access);
   end Remove_From_Bag;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Undo_Info) is
      Empty_Undo : constant Undo_Info := (others => <>);
   begin
      This := Empty_Undo;
   end Reset;

   -----------------------------
   -- Select_Random_Insertion --
   -----------------------------

   procedure Select_Random_Insertion (This  : in     Object;
                                      Bag   : in     Bag_Key;
                                      Prev  :    out Task_Context_Ptr;
                                      Curr  :    out Task_Context_Ptr;
                                      Next  :    out Task_Context_Ptr)
   is
      procedure Do_It (Key   : in Bag_Key;
                       Tasks : in Solution_Context_Bags.Object)
      is
         pragma Unreferenced (Key);

         Dst_Idx   : Positive;

         Before    : Boolean;
      begin
         if Tasks.Length = 0 then
            Prev := null;
            Curr := null;
            Next := null;
            return;
         end if;

         Dst_Idx := Random.Get_Integer (Tasks.First, Tasks.Last);

         Curr := This.Ptr (Tasks.Vector (Dst_Idx));

         Before := Random.Get_Integer (1, 2) = 1;

         --  Chose before *or* after the target
         if Before then
            --  Before
            Next  := +This.Ptr (Tasks.Vector (Dst_Idx));
            Prev  := This.Get_Task_Context (Next.Prev);
         else
            --  After
            Prev  := +This.Ptr (Tasks.Vector (Dst_Idx));
            Next  := This.Get_Task_Context (Prev.Next);
         end if;
      end Do_It;
   begin
      Solution_Context_Bag_Maps.Query_Element
        (This.Bags.Find (Bag),
         Do_It'Access);
   end Select_Random_Insertion;

   ---------------------------
   -- Select_Random_Context --
   ---------------------------

   function Select_Random_Context (This : in     Object;
                                   Bag  : in     Bag_Key) return Solution_Context_Ptr
   is
      Result : Solution_Context_Ptr;

      procedure Do_It (Key   : in Bag_Key;
                       Ctxts : in Solution_Context_Bags.Object)
      is
         pragma Unreferenced (Key);
         Src_Idx   : constant Positive :=
                       Random.Get_Integer (Ctxts.First, Ctxts.Last);
      begin
         if Ctxts.Is_Empty then
            Result := null;
         else
            Result := This.Ptr (Ctxts.Vector (Src_Idx));
         end if;
      end Do_It;
   begin
      Solution_Context_Bag_Maps.Query_Element
        (This.Bags.Find (Bag),
         Do_It'Access);
      return Result;
   end Select_Random_Context;

   ------------------------
   -- Select_Random_Task --
   ------------------------

   function Select_Random_Task (This : in     Object;
                                Bag  : in     Bag_Key) return Task_Context_Ptr
   is
   begin
      return Task_Context_Ptr (This.Select_Random_Context (Bag));
   end Select_Random_Task;

   --------------------
   -- Set_Assignment --
   --------------------

   procedure Set_Assignment (This      : in out Object;
                             Ass       : in     Cr.Assignment.Object;
                             Criterion : in Assignment_Criteria)
   is
      New_Ass       : Cr.Assignment.Object := Ass;
      Pending_Tasks : Htn.Tasks.Maps.Map;
      L             : constant Htn.Tasks.Containers.Lists.List :=
                        Htn.Plan.Enumerate_Tasks
                          (Htn.Plan.Utils.Random.Get_Any_Expansion
                             (Ass.Freeze_Plan (This.Context.Ref.Plan)),
                           Primitive => True,
                           Pending   => True);
      procedure Ins (I : Htn.Tasks.Containers.Lists.Cursor) is
         use Htn.Tasks.Containers.Lists;
      begin
         Pending_Tasks.Insert (Element (I).Get_Id, Element (I));
      end Ins;

      procedure Process_Agent (I : Cr.Agent.Containers.Lists.Cursor) is
         use Cr.Agent.Containers.Lists;
         use Htn.Tasks.Containers.Lists;
         A : constant Cr.Agent.Object'Class  := Element (I);
         T : constant Htn.Tasks.Containers.Lists.List   := A.Get_Tasks;
         J :          Htn.Tasks.Containers.Lists.Cursor := T.First;
         Prev : Htn.Tasks.Task_Id := No_Task;
      begin
         --  Create its task assignment and bag
         declare
            Ag_Ctx : Agent_Context := (Solution_Context with
                                       Agent_Name => +A.Get_Name);
         begin
            Add_To_Bag (This, Ag_Ctx, All_Agents);
            This.Create_Empty_Bag (Agent_Tasks_Bag (A.Get_Name));
            This.Contexts.Insert (Agent_Key (A.Get_Name), Ag_Ctx);
         end;

         --  Create task contexts for tasks in our plan
         while Has_Element (J) loop
            if This.Context.Ref.Plan.Contains (Element (J).Get_Id) and then
              not Htn.Plan_Node.Get_Finished (This.Context.Ref.Plan.Get_Node
                                              (Element (J).Get_Id))
            then
               declare
                  C : aliased Task_Context;
                  function Next_Id (K : Task_Lists.Cursor) return Htn.Tasks.Task_Id
                  is
                     Plan   : Htn.Plan.Object renames This.Context.Ref.Plan;
                     Nx     : constant Task_Lists.Cursor := Next (K);
                  begin
                     if not Has_Element (Nx) then
                        return No_Task;
                     elsif Plan.Contains (Element (Nx).Get_Id) and then
                        not Htn.Plan_Node.Get_Finished
                          (Plan.Get_Node (Element (Nx).Get_Id))
                     then
                        return Element (Nx).Get_Id;
                     else
                        return Next_Id (Nx);
                        pragma Optimization_Opportunity ("Unroll recursivity");
                     end if;
                  end Next_Id;
               begin
                  C.Job  := Element (J).Get_Id;
                  C.Prev := Prev;
                  Prev   := C.Job;
                  C.Next := Next_Id (J);
                  Set_Attribute (C'Access, Owner, Cr.Agent.Get_Name (A));

                  --  Add to all tasks
                  This.Add_To_Bag (C, All_Assigned_Tasks);
                  --  Add to its agent
                  This.Add_To_Bag (C, Agent_Tasks_Bag (A.Get_Name));

                  This.Contexts.Insert
                    (Solution_Context_Key (Task_Key (C.Job)), C);

                  --  Create OR contexts
                  This.Add_Or_Contexts
                    (This.Context.Ref.Plan.Get_Node (C.Job));
               end;
            else
               Log ("Set assignment: discarding unplanned task",
                    Debug, Log_Section);
            end if;
            Next (J);
         end loop;
      end Process_Agent;

      procedure Remove_Agent_Tasks (I : Cr.Agent.Containers.Lists.Cursor) is
         use Cr.Agent.Containers.Lists;
         use Htn.Tasks.Containers.Lists;
         A : constant Cr.Agent.Object'Class  := Element (I);
         T : constant Task_Lists.List   := A.Get_Tasks;
         J :          Task_Lists.Cursor := T.First;
      begin
         --  Ensure we have the agent
         if not This.Context.Ref.all.Agents.Contains (A.Get_Name) then
            return;
         end if;

         while Has_Element (J) loop
            Pending_Tasks.Exclude (Element (J).Get_Id);
            Next (J);
         end loop;
      end Remove_Agent_Tasks;


   begin
      if not Ass.Is_Valid then
         raise Constraint_Error with "Can't set from invalid assignment";
      end if;

      Clear_Dynamic_Part (This);

      --  Keep mapped tasks
      L.Iterate (Ins'Access);

      --  Remove assigned tasks
      declare
         Agents : constant Cr.Agent.Containers.Lists.List := Ass.Get_Agents;
      begin
         Cr.Agent.Containers.Lists.Iterate (Agents, Remove_Agent_Tasks'Access);
      end;

      --  At this point, the Pending_Tasks lists contains only tasks in the plan
      --  that weren't in the assignment received. We are going to assign them
      --  in some greedy fashion.

      Log ("There are" & Pending_Tasks.Length'Img & " new pending tasks.",
           Debug, Detail_Section);

      --  Do something with unassigned plan tasks
      while not Pending_Tasks.Is_Empty loop
         declare
            New_New_Ass : Cr.Assignment.Object;
            Success     : Boolean;
         begin
            pragma Assert (New_New_Ass.Is_Valid);
--              Log (This.Context.Ref.Agents.Length'Img, Always);
--              Log (Htn.Tasks.Maps.First_Element (Pending_Tasks).To_String, Always);
            Tasks.Insertions.Greedy
              (New_Ass,
               Htn.Tasks.Maps.First_Element (Pending_Tasks),
               This.Context.Ref.Costs.Ref.all,
               Criterion,
               New_New_Ass,
               Success);

            if not Success then
               Log ("Set_Assignment: cannot assign task " &
                    To_String (Integer (Pending_Tasks.First_Element.Get_Id)) &
                    ":" & Pending_Tasks.First_Element.To_String &
                    "," & Pending_Tasks.Length'Img & " remaining",
                    Error, Log_Section);
               This.Valid := False;
               raise Constraint_Error;
               return;
            else
               Log ("Set_Assignment: assigned task " &
                    To_String (Integer (Pending_Tasks.First_Element.Get_Id)) &
                    "-" & Pending_Tasks.First_Element.To_String, Debug,
                    Detail_Section);
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
         Agents : constant Cr.Agent.Containers.Lists.List := New_Ass.Get_Agents;
      begin
         Cr.Agent.Containers.Lists.Iterate (Agents, Process_Agent'Access);
      end;

      Log ("Set_Assignment: Assigned tasks reinserted",
           Debug, Section => Detail_Section);

      Reevaluate_Costs (This);

      Log ("Set_Assignment: Costs reevaluated",
           Debug, Section => Detail_Section);

      --  This.To_Assignment.Print_Assignment;
      --  This.Debug_Dump_Contexts;
   end Set_Assignment;

   ---------------
   -- Set_Costs --
   ---------------

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Cache.Object'Class)
   is
      C : constant Static_Context_Access := This.Context.Ref;
      Was_Valid : Boolean;
   begin
      C.Costs.Set (Costs);

      Was_Valid := This.Valid;
      Reevaluate_Costs (This);
      if Was_Valid and not This.Valid then
         Log ("Cost change made assignment invalid!", Warning, Log_Section);
      end if;
   end Set_Costs;

   procedure Set_Criterion (This      : in out Object;
                            Criterion : in     Assignment_Criteria)
   is
      C : constant Static_Context_Access := This.Context.Ref;
   begin
      C.Criterion := Criterion;
   end Set_Criterion;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute (Context : not null access Solution_Context'Class;
                            Attr    : in Solution_Context_Attributes;
                            Val     : in String)
   is
      use Attribute_Maps;
   begin
      if Attr = Owner then
         if Val = "" then
            raise Program_Error;
         end if;
      end if;
      Include (Context.Attributes, Attr, Val);
   end Set_Attribute;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object;
                        Assign : in     Boolean := True)
   is
      C        : Static_Context_Access renames This.Context.Ref;
      Prev_Ass : Assignment.Object;
      Was_Valid : Boolean;
   begin
      if Assign then
         Was_Valid := This.Valid;
         Prev_Ass := This.To_Assignment;
         if Was_Valid and not Prev_Ass.Is_Valid then
            raise Constraint_Error with "Mismatch in validity";
         elsif not Was_Valid then
            raise Constraint_Error with "Will not be able to assign from invalid assignment";
         end if;
      end if;
      Clear_Dynamic_Part (This);
      C.Plan := Htn.Plan.Inflate (Plan);
      if Assign then
         This.Set_Assignment (Prev_Ass,
                              This.Context.Ref.Criterion);
      end if;
   end Set_Tasks;

   --------------
   -- Task_Key --
   --------------

   function Task_Key (Id : in Htn.Tasks.Task_Id) return Solution_Context_Key is
   begin
      return Solution_Context_Key (+("T:" & Id'Img));
   end Task_Key;

   -------------------
   -- To_Assignment --
   -------------------

   function To_Assignment   (This : in Object) return Cr.Assignment.Object is
      Result : Cr.Assignment.Object;

      function Find_First (Agent : in Agent_Id) return Task_Context_Ptr is
         Curr  : Task_Context;
         Found : Boolean := False;
         use Solution_Context_Maps;
         I     : Cursor := This.Contexts.First;
      begin
         --  Locate any:
         while Has_Element (I) loop
            if Element (I) in Task_Context then
               if Get_Attribute (Element (I), Owner) = String (Agent) then
                  Curr  := Task_Context (Element (I));
                  Found := True;
                  exit;
               end if;
            end if;
            Next (I);
         end loop;

         --  Go back to first one:
         if Found then
            while Curr.Prev /= No_Task loop
               Curr := Task_Context
                 (Element (This.Contexts.Find (Task_Key (Curr.Prev))));
            end loop;
            return This.Ptr (Task_Key (Curr.Job));
         else
            return null;
         end if;
      end Find_First;

      procedure Assign_Agent (I : Agent_Maps.Cursor) is
         Agent : constant Cr.Agent.Object'Class := Agent_Maps.Element (I);
         Curr : Task_Context_Ptr := Find_First (Agent.Get_Name);
      begin
         --  Add the empty agent so all appear in the assignment, even the ones
         --  without tasks:
         Assignment.Set_Agent (Result, Agent_Maps.Element (I));
         while Curr /= null loop
            Assignment.Add
              (Result, Agent,
               Htn.Plan.Get_Task (This.Context.Ref.Plan, Curr.Job).all);
            exit when Curr.Next = No_Task;
            Curr := This.Get_Task_Context (Curr.Next);
         end loop;
      end Assign_Agent;
   begin
--      This.Debug_Dump_Contexts;
      Agent_Maps.Iterate (This.Context.Ref.Agents, Assign_Agent'Access);
      Result.Set_Valid (This.Valid);
      Log ("Converted to assignment; Valid: " & This.Valid'Img,
           Debug, Log_Section);
      return Result;
   end To_Assignment;

   ----------
   -- Undo --
   ----------

   procedure Undo (This : in out Object) is
   begin
      if This.Undo.Handle.Is_Valid then
         This.Undoer (This, This.Undo);
         This.Valid := This.Was_Valid;
         This.Undo.Handle.Clear;
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
      case Undo.Handle.Get.Kind is
         when From_Scratch =>
            This.Set_Assignment (Undo.Handle.Get.Ass,
                                 This.Context.Ref.Criterion);
         when Identity =>
            null;
         when others =>
            raise Program_Error;
      end case;
   end Undo_From_Scratch;

   -------------------
   -- Undo_Identity --
   -------------------

   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info) is
      pragma Unreferenced (This, Undo);
   begin
      null;
   end Undo_Identity;

   -----------------------------
   -- Evaluate_Cost_Inserting --
   -----------------------------

   function Evaluate_Cost_Inserting
     (This                : in Object;
      Prev_In_List        : in Task_Context_Ptr;
      Curr_To_Be_Inserted : in Htn.Tasks.Task_Id;
      Next_In_List        : in Task_Context_Ptr;
      New_Owner           : in Agent_Id) return Cr.Costs
   is
      Prev : Task_Context_Ptr renames Prev_In_List;
      Next : Task_Context_Ptr renames Next_In_List;
      Curr : Htn.Tasks.Task_Id renames Curr_To_Be_Inserted;

      Plus_1   : Costs := 0.0;
      Plus_2   : Costs := 0.0;
      Minus    : Costs := 0.0;
      Cost     : constant Costs := Acm.Element (This.Agent_Costs.Find (New_Owner));
      Cm       : Cost_Cache.Object'Class renames This.Context.Ref.Costs.Ref.all;
      Pr, Ne   : Htn.Tasks.Task_Id := No_Task;
   begin

      if Prev /= null then
         Pr := Prev.Job;
      end if;
      if Next /= null then
         Ne := Next.Job;
      end if;

      Plus_1 := Cost_Cache.Get_Cost (Cm,
                                     New_Owner,
                                     Pr,
                                     Curr);

      if Next /= null then
         Plus_2 := Cost_Cache.Get_Cost (Cm,
                                        New_Owner,
                                        Curr,
                                        Ne);

         Minus   := Cost_Cache.Get_Cost (Cm,
                                         New_Owner,
                                         Pr,
                                         Ne);
      end if;

      Log ("Inserting: [Plus1/Plus2/Minus] = " &
           To_String (Plus_1, 10) & " " &
           To_String (Plus_2, 10) & " " &
           To_String (Minus, 10), Debug, Detail_Section);

      if Plus_1 = Cr.Infinite or else Plus_2 = Cr.Infinite then
         return Cr.Infinite;
      elsif Minus = Cr.Infinite then
         raise Program_Error with "Minus shouldn't never equal Inf.";
      else
         return Cr.Evaluate (This.Context.Ref.Criterion,
                             Minmax => Cost + Plus_1 + Plus_2 - Minus,
                             Minsum =>        Plus_1 + Plus_2 - Minus);
      end if;
   exception
      when E : others =>
         Log ("Evaluate_Cost_Inserting [Plus1/Plus2/Minus]: " &
              Cr.Image (Plus_1) & " " &
              Cr.Image (Plus_2) & " " &
              Cr.Image (Minus), Error, Log_Section);
         Log ("Exception: " & Report (E), Error, Log_Section);
         raise;
   end Evaluate_Cost_Inserting;

   ----------------------------
   -- Update_Costs_Inserting --
   ----------------------------

   procedure Update_Costs_Inserting
     (This                : in out Object;
      Prev_In_List        : in     Task_Context_Ptr;
      Curr_To_Be_Inserted : in     Task_Context_Ptr;
      Next_In_List        : in     Task_Context_Ptr;
      New_Owner           : in     String)
   is
      Prev : Task_Context_Ptr renames Prev_In_List;
      Next : Task_Context_Ptr renames Next_In_List;
      Curr : Task_Context_Ptr renames Curr_To_Be_Inserted;

      Agent    : constant Agent_Id := Agent_Id (New_Owner);
      Plus_1   : Costs := 0.0;
      Plus_2   : Costs := 0.0;
      Minus    : Costs := 0.0;
      Cost     : Costs := Acm.Element (This.Agent_Costs.Find (Agent));
      Cm       : Cost_Cache.Object'Class renames This.Context.Ref.Costs.Ref.all;

      Pr, Ne   : Htn.Tasks.Task_Id := No_Task;
   begin
      This.Agent_Costs.Delete (Agent);
      This.MinMax.Delete ((Cost, +String (Agent)));

      if Prev /= null then
         Pr := Prev.Job;
      end if;
      if Next /= null then
         Ne := Next.Job;
      end if;

      Plus_1 := Cost_Cache.Get_Cost (Cm,
                                      New_Owner,
                                      Pr,
                                      Curr.Job);

      if Next /= null then
         Plus_2 := Cost_Cache.Get_Cost (Cm,
                                         New_Owner,
                                         Curr.Job,
                                         Ne);

         Minus   := Cost_Cache.Get_Cost (Cm,
                                          New_Owner,
                                          Pr,
                                          Ne);
      end if;

      if Plus_1 = Infinite then
         Log ("Plus_1 invalidating ass", Debug, Detail_Section);
         Plus_1 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Plus_2 = Infinite then
         Log ("Plus_2 invalidating ass", Debug, Detail_Section);
         Plus_2 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Minus = Infinite then
         Log ("Minus invalidating ass", Debug, Detail_Section);
         Minus := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;

      Log ("Inserting: [Plus1/Plus2/Minus] = " &
           To_String (Plus_1, 16) & " " &
           To_String (Plus_2, 16) & " " &
           To_String (Minus, 16), Debug, Detail_Section);

      Log ("Inserting [Prev/After] " & To_String (Cost, 16) & " " &
           To_String (Cost + Plus_1 + Plus_2 - Minus, 16),
           Debug, Detail_Section);
      Cost := Cost + Plus_1 + Plus_2 - Minus;

      This.Agent_Costs.Insert (Agent_Id (New_Owner), Cost);
      This.MinMax.Insert ((Cost, +New_Owner));
      This.Minsum := This.Minsum + Plus_1 + Plus_2 - Minus;
   end Update_Costs_Inserting;

   ---------------------------
   -- Update_Costs_Removing --
   ---------------------------

   procedure Update_Costs_Removing
     (This               : in out Object;
      Prev_To_Be_Kept    : in     Task_Context_Ptr;
      Curr_To_Be_Deleted : in     Task_Context_Ptr;
      Next_To_Be_Kept    : in     Task_Context_Ptr;
      Former_Owner       : in     String)
   is
      Prev : Task_Context_Ptr renames Prev_To_Be_Kept;
      Next : Task_Context_Ptr renames Next_To_Be_Kept;
      Curr : Task_Context_Ptr renames Curr_To_Be_Deleted;

      Agent : constant Agent_Id := Agent_Id (Former_Owner);
      Minus_1  : Costs := 0.0;
      Minus_2  : Costs := 0.0;
      Plus     : Costs := 0.0;
      Cost     : Costs := Acm.Element (This.Agent_Costs.Find (Agent));
      Cm       : Cost_Cache.Object'Class renames This.Context.Ref.Costs.Ref.all;

      Pr, Ne   : Htn.Tasks.Task_Id := No_Task;
   begin
      This.Agent_Costs.Delete (Agent);
      This.MinMax.Delete ((Cost, +String (Agent)));

      if Prev /= null then
         Pr := Prev.Job;
      end if;
      if Next /= null then
         Ne := Next.Job;
      end if;

      Minus_1 := Cost_Cache.Get_Cost (Cm,
                                       String (Agent),
                                       Pr,
                                       Curr.Job);

      Minus_2 := Cost_Cache.Get_Cost (Cm,
                                       String (Agent),
                                       Curr.Job,
                                       Ne);

      Plus    := Cost_Cache.Get_Cost (Cm,
                                       String (Agent),
                                       Pr,
                                       Ne);


      if Minus_1 = Infinite then
         Log ("Minus_1 invalidating ass", Debug, Detail_Section);
         Minus_1 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Minus_2 = Infinite then
         Log ("Minus_2 invalidating ass", Debug, Detail_Section);
         Minus_2 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Plus = Infinite then
         Log ("Plus invalidating ass", Debug, Detail_Section);
         Plus := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;

      Log ("Removing: [Minus1/Minus2/Plus] = " &
           To_String (Minus_1, 16) & " " &
           To_String (Minus_2, 16) & " " &
           To_String (Plus, 16), Debug, Detail_Section);

      Log ("Removing [Prev/After] " & To_String (Cost, 16) & " " &
           To_String (Cost - Minus_1 - Minus_2 + Plus, 16),
           Debug, Detail_Section);

      Log ("Removing costs: Prev:" & Pr'Img & "; Next:" & Ne'Img &
           "; Curr:" & Curr.Job'Img,
           Debug, Detail_Section);
--        Log ("Removing task: Minus_1 = " & To_String (Minus_1) &
--             "; Minus_2 = " & To_String (Minus_2) &
--             "; Plus = " & To_String (Plus), Debug, Log_Section);

      Cost := Cost - Minus_1 - Minus_2 + Plus;

      This.Agent_Costs.Insert (Agent, Cost);
      This.MinMax.Insert ((Cost, +String (Agent)));
      This.Minsum := This.Minsum - Minus_1 - Minus_2 + Plus;
   end Update_Costs_Removing;

   ---------
   -- Key --
   ---------

   function Key (This : in Agent_Context) return Solution_Context_Key is
   begin
      return Agent_Key (+This.Agent_Name);
   end Key;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (This : in Agent_Context) is
   begin
      Log ("Agent: " & (+This.Agent_Name), Always, Log_Section);
   end Debug_Dump;

   function Or_Key (This : in String) return Solution_Context_Key is
   begin
      return Solution_Context_Key (U ("OR:" & This));
   end Or_Key;

   ---------
   -- Key --
   ---------

   function Key (This : in Or_Context) return Solution_Context_Key is
      use Htn.Plan_Node;
   begin
      return Or_Key (Get_Id (This.Node));
   end Key;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (This : in Or_Context) is
      use Htn.Plan_Node;
   begin
      Log ("OR node: " & Get_Id (This.Node) &
           "; Branch taken: " & Get_Id (This.Branch), Always, Log_Section);
   end Debug_Dump;

end Agpl.Cr.Mutable_Assignment;
