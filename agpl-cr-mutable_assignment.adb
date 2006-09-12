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
with Agpl.Cr.Agent.Handle;
with Agpl.Cr.Assigner.Greedy_Minmax_Exhaustive;
with Agpl.Cr.Plan_Assigner;
with Agpl.Cr.Plan_Assigner.Greedy1;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Htn.Plan_Node;
with Agpl.Htn.Plan.Utils;
with Agpl.Htn.Plan.Utils.Random;
with Agpl.Htn.Tasks.Maps;
with Agpl.Random;
with Agpl.Trace;   use Agpl.Trace;

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;

package body Agpl.Cr.Mutable_Assignment is

   Expensive_Checks : constant Boolean := False;

   File : constant String := "[Mutable_Assignment] ";

   use type Htn.Tasks.Task_Id;
   use type Optimization.Cost;
   use type Optimization.Annealing.Probability;

   package Acm renames Agent_Cost_Maps;

   No_Task : Htn.Tasks.Task_Id renames Htn.Tasks.No_Task;

   function S is new Conversions.To_Str (Optimization.Annealing.Probability);
   function To_String is new Conversions.To_Str (Cr.Costs);

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

   function S (Key : in Solution_Context_Key) return String is
   begin
      return +Ustring (Key);
   end S;

   ---------------
   -- Add_Agent --
   ---------------

   procedure Add_Agent (This : in out Object; A : in Cr.Agent.Object'Class) is
   begin
      This.Context.Ref.all.Agents.Include (A.Get_Name, A);
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
                            Undo : in out Undo_Info)
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
                             Desc :    out Ustring;
                             Undo : in out Undo_Info)
   is
      A : Cr.Assignment.Object := This.To_Assignment;
   begin
      Undo.Ass := A;
      Desc := +"Heuristic 1";
      declare
         use Cr.Assignment;
         New_Assignment : constant Cr.Assignment.Object :=
                            Cr.Assigner.Greedy_MinMax_Exhaustive.Assign
                              ((Cr.Assigner.Object with Keep_Order => True),
                               Get_Agents_Without_Tasks (A),
                               Get_All_Tasks (A),
                               This.Context.Ref.Costs);
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

   --------------------
   -- Do_Heuristic_2 --
   --------------------

   procedure Do_Heuristic_2 (This : in out Object;
                             Desc :    out Ustring;
                             Undo : in out Undo_Info)
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

   ----------------------
   -- Do_Agent_Reorder --
   ----------------------

   procedure Do_Agent_Reorder (This : in out Object;
                               Desc :    out Ustring;
                               Undo : in out Undo_Info)
   is
      Agent : constant Agent_Id :=
                Agent_Id
                  (+Agent_Context
                     (This.Select_Random_Context (All_Agents).all).Agent_Name);
   begin
      Desc     := +"AGENT REORDER N²";
--      Log ("Reordering agent", Always);
--      This.To_Assignment.Print_Assignment;

      declare
         New_Ass : Assignment.Object := This.To_Assignment;
         Ag      : Cr.Agent.Object'Class := New_Ass.Get_Agent (String (Agent));
         Tasks   : Task_Lists.List := Ag.Get_Tasks;
         U       : Undo_Info;
      begin
         U.Ass    := New_Ass;
         Undo     := U;
         Ag.Clear_Tasks;
         while not Tasks.Is_Empty loop
            declare
               New_Ag : Cr.Agent.Handle.Object;
               Cd, Ct : Cr.Costs;
               Ok     : Boolean;
            begin
               Cr.Tasks.Insertions.Greedy (Ag,
                                           Tasks.First_Element,
                                           New_Ag,
                                           Cd, Ct, Ok);
               if not Ok then
                  Log ("Failed to reorder agent tasks", Warning, Log_Section);
                  This.Do_Identity (Desc, Undo);
                  return;
               else
                  Ag := New_Ag.Get;
                  Tasks.Delete_First;
               end if;
            end;
         end loop;
         New_Ass.Set_Agent (Ag);
         New_Ass.Set_Valid;
--         New_Ass.Print_Summary;
         This.Set_Assignment (New_Ass, This.Context.Ref.Criterion);
      end;
   end Do_Agent_Reorder;

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
              " and before" & Src_Ptr.Next'Img,
              Debug, Detail_Section);

         declare
            Cost : Costs;
         begin
            pragma Remove_Expensive_Check;
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

   ---------------------
   -- Do_Auction_Task --
   ---------------------

   procedure Do_Auction_Task (This : in out Object;
                              Desc :    out Ustring;
                              Undo : in out Undo_Info)
   is
   begin
      if This.Num_Assigned_Tasks <= 1 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "LOG AUCTION";

      declare
         use Ada.Numerics.Elementary_Functions;
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);
         Checks   : constant Positive :=
                      Natural (Log (Float (This.Num_Assigned_Tasks))) + 1;

         Best_Prev,
         Best_Next   : Task_Context_Ptr;
         Best_Cost   : Costs := Infinite;
         Best_Name   : Ustring;
      begin
         This.Add_Undo_Move (Src, Undo);
         This.Do_Remove_Task (Src);

         Log ("Checking" & Checks'Img & " of" & This.Num_Assigned_Tasks'Img &
              " possible insertions", Debug, Detail_Section);

         for I in 1 .. Checks loop
            declare
               Curr_Target,
               Curr_Prev,
               Curr_Next : Task_Context_Ptr;
               Curr_Cost : Costs;
            begin
               This.Select_Random_Insertion (All_Assigned_Tasks,
                                             Curr_Prev,
                                             Curr_Target,
                                             Curr_Next);
               Curr_Cost := This.Evaluate_Cost_Inserting
                 (Curr_Prev,
                  Src_Copy.Job,
                  Curr_Next,
                  Agent_Id (Get_Attribute (Curr_Target, Owner)));

               if Curr_Cost < Best_Cost then
                  Best_Cost := Curr_Cost;
                  Best_Prev := Curr_Prev;
                  Best_Next := Curr_Next;
                  Best_Name := +String (Get_Attribute (Curr_Target, Owner));
               end if;
            end;
         end loop;

         if Best_Cost < Cr.Infinite then
            This.Do_Insert_Task (Best_Prev,
                                 Src_Copy,
                                 Best_Next,
                                 Agent_Id (+Best_Name));
         else
            This.Do_Insert_Task (This.Get_Task_Context (Src_Copy.Prev),
                                 Src_Copy,
                                 This.Get_Task_Context (Src_Copy.Next),
                                 Get_Attribute (Src_Copy, Owner));
            This.Do_Identity (Desc, Undo);
         end if;
      end;
   end Do_Auction_Task;

   ----------------------------
   -- Do_Guided_Auction_Task --
   ----------------------------

   procedure Do_Guided_Auction_Task (This : in out Object;
                                     Desc :    out Ustring;
                                     Undo : in out Undo_Info)
   is
   begin
      if This.Num_Assigned_Tasks <= 1 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "GUIDED+LOG AUCTION";

      declare
         use Ada.Numerics.Elementary_Functions;
         Worst_Agent : constant Agent_Id :=
                         Agent_Id (+This.Minmax.Last_Element.Agent);
         Best_Agent  : constant Agent_Id :=
                         Agent_Id (+This.Minmax.First_Element.Agent);
         Src         : Task_Context_Ptr :=
                         This.Select_Random_Task (Agent_Tasks_Bag (Worst_Agent));
         Src_Copy    : Task_Context := Task_Context (Src.all);
         Checks      : constant Positive :=
                         Natural (Log (Float (This.Num_Assigned_Tasks))) + 1;

         Best_Prev,
         Best_Next   : Task_Context_Ptr;
         Best_Cost   : Costs := Infinite;
         Best_Name   : Ustring;
      begin
         This.Add_Undo_Move (Src, Undo);
         This.Do_Remove_Task (Src);

         Log ("Checking" & Checks'Img & " of" & This.Num_Assigned_Tasks'Img &
              " possible insertions", Debug, Detail_Section);

         for I in 1 .. Checks loop
            declare
               Curr_Target,
               Curr_Prev,
               Curr_Next : Task_Context_Ptr;
               Curr_Cost : Costs;
            begin
               This.Select_Random_Insertion (Agent_Tasks_Bag (Best_Agent),
                                             Curr_Prev,
                                             Curr_Target,
                                             Curr_Next);
               Curr_Cost := This.Evaluate_Cost_Inserting
                 (Curr_Prev,
                  Src_Copy.Job,
                  Curr_Next,
                  Agent_Id (Get_Attribute (Curr_Target, Owner)));
               if Curr_Cost < Best_Cost then
                  Best_Cost := Curr_Cost;
                  Best_Prev := Curr_Prev;
                  Best_Next := Curr_Next;
                  Best_Name := +String (Get_Attribute (Curr_Target, Owner));
               end if;
            end;
         end loop;
         if Best_Cost < Cr.Infinite then
            This.Do_Insert_Task (Best_Prev,
                                 Src_Copy,
                                 Best_Next,
                                 Agent_Id (+Best_Name));
         else
            This.Do_Insert_Task (This.Get_Task_Context (Src_Copy.Prev),
                                 Src_Copy,
                                 This.Get_Task_Context (Src_Copy.Next),
                                 Get_Attribute (Src_Copy, Owner));
            This.Do_Identity (Desc, Undo);
         end if;
      end;
   end Do_Guided_Auction_Task;

   ------------------
   -- Do_Move_Task --
   ------------------

   procedure Do_Move_Task (This : in out Object;
                           Desc :    out Ustring;
                           Undo : in out Undo_Info)
   is
      Src      : Task_Context_Ptr :=
                   This.Select_Random_Task (All_Assigned_Tasks);
      Src_Copy : Task_Context := Task_Context (Src.all);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "MOVE";

      This.Add_Undo_Move (Src, Undo);
      This.Do_Remove_Task (Src);

      declare
         Target    : Task_Context_Ptr;

         New_Prev,
         New_Next  : Task_Context_Ptr;
      begin
         This.Select_Random_Insertion (All_Assigned_Tasks,
                                       New_Prev,
                                       Target,
                                       New_Next);
         declare
            New_Owner : constant Agent_Id := Get_Attribute (Target, Owner);
         begin
            Do_Insert_Task (This,
                            New_Prev,
                            Src_Copy,
                            New_Next,
                            New_Owner);
         end;
      end;
   end Do_Move_Task;

   ---------------------------------
   -- Do_Move_Task_Changing_Owner --
   ---------------------------------

   procedure Do_Move_Task_Changing_Owner (This : in out Object;
                                          Desc :    out Ustring;
                                          Undo : in out Undo_Info)
   is
   begin
      if This.Num_Assigned_Tasks <= 1 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "MOVE+OWNER";

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);

         New_Owner : constant Agent_Id :=
                       Agent_Id
                         (+ Agent_Context
                            (This.Select_Random_Context
                               (All_Agents).all).Agent_Name);

      begin
         This.Add_Undo_Move (Src, Undo);
         This.Do_Remove_Task (Src);

         declare
            Prev, Curr, Next : Task_Context_Ptr;
         begin
            This.Select_Random_Insertion
              (Agent_Tasks_Bag (New_Owner),
               Prev,
               Curr,
               Next);
            This.Do_Insert_Task (Prev,
                                 Src_Copy,
                                 Next,
                                 New_Owner);
         end;
      end;
   end Do_Move_Task_Changing_Owner;

   ----------------------------------------
   -- Do_Guided_Move_Task_Changing_Owner --
   ----------------------------------------

   procedure Do_Guided_Move_Task_Changing_Owner (This : in out Object;
                                                 Desc :    out Ustring;
                                                 Undo : in out Undo_Info)
   is
   begin
      if This.Num_Assigned_Tasks <= 1 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "MOVE+GUIDED+OWNER";

      declare
         Worst_Agent : constant Agent_Id :=
                         Agent_Id (+This.Minmax.Last_Element.Agent);
         Src         : Task_Context_Ptr :=
                         This.Select_Random_Task (Agent_Tasks_Bag (Worst_Agent));
         Src_Copy : Task_Context := Task_Context (Src.all);
         New_Owner : constant Agent_Id :=
                       Agent_Id
                         (+ Agent_Context
                            (This.Select_Random_Context
                               (All_Agents).all).Agent_Name);
      begin
         This.Add_Undo_Move (Src, Undo);
         This.Do_Remove_Task (Src);

         declare
            Prev, Curr, Next : Task_Context_Ptr;
         begin
            This.Select_Random_Insertion
              (Agent_Tasks_Bag (New_Owner),
               Prev,
               Curr,
               Next);
            This.Do_Insert_Task (Prev,
                                 Src_Copy,
                                 Next,
                                 New_Owner);
         end;
      end;
   end Do_Guided_Move_Task_Changing_Owner;

   -------------------
   -- Do_Swap_Order --
   -------------------

   procedure Do_Swap_Order (This : in out Object;
                            Desc :    out Ustring;
                            Undo : in out Undo_Info) is
   begin
      if This.Num_Assigned_Tasks <= 1 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "SWAP ORDER";

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);
         Next     : constant Task_Context_Ptr :=
                      This.Get_Task_Context (Src.Next);
      begin
         if Next /= null then
            This.Add_Undo_Move (Src, Undo);
            This.Do_Remove_Task (Src);
            This.Do_Insert_Task (Next,
                                 Src_Copy,
                                 This.Get_Task_Context (Next.Next),
                                 Agent_Id (Get_Attribute (Next, Owner)));
         else
            Do_Identity (This, Desc, Undo);
         end if;
      end;
   end Do_Swap_Order;

   -------------------
   -- Do_Swap_Tasks --
   -------------------

   procedure Do_Swap_Tasks (This : in out Object;
                            Desc :    out Ustring;
                            Undo : in out Undo_Info) is
   begin
      if This.Num_Assigned_Tasks <= 2 then
         Do_Identity (This, Desc, Undo);
         return;
      end if;

      Desc := + "SWAP ANY";

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context     := Task_Context (Src.all);
         Prev_1   : constant Htn.Tasks.Task_id := Src.Prev;
         Next_1   : constant Htn.Tasks.Task_id := Src.Next;
         Owner_1  : constant Agent_Id          :=
                      Agent_Id (Get_Attribute (Src, Owner));
      begin
         This.Add_Undo_Move (Src, Undo);
         This.Do_Remove_Task (Src);

         declare
            Target      : Task_Context_Ptr;

            New_Prev,
            New_Next    : Task_Context_Ptr;
            Watchdog    : Natural := 0;
         begin
            loop
               This.Select_Random_Insertion (All_Assigned_Tasks,
                                             New_Prev,
                                             Target,
                                             New_Next);
               exit when Target.Job /= Prev_1 and then
                         Target.Job /= Next_1;
               Watchdog := Watchdog + 1;
               if Watchdog > 100 then
                  raise Program_Error with "Moving target failed";
               end if;
            end loop;

            declare
               Target_Copy : Task_Context      := Task_Context (Target.all);
               New_Owner   : constant Agent_Id := Get_Attribute (Target, Owner);
            begin
               Do_Insert_Task (This,
                               New_Prev,
                               Src_Copy,
                               New_Next,
                               New_Owner);

               This.Add_Undo_Move (Target, Undo);
               This.Do_Remove_Task (Target);

               if Prev_1 /= No_Task then
                  declare
                     Prev : constant Task_Context_Ptr :=
                              This.Get_Task_Context (Prev_1);
                     Next : constant Task_Context_Ptr :=
                              This.Get_Task_Context (Prev.Next);
                  begin
                     This.Do_Insert_Task
                       (Prev, Target_Copy, Next, Owner_1);
                  end;
               elsif Next_1 /= No_Task then
                  This.Do_Insert_Task
                    (This.Get_Task_Context
                       (This.Get_Task_Context (Next_1).Prev),
                     Target_Copy,
                     This.Get_Task_Context (Next_1),
                     Owner_1);
               else
                  This.Do_Insert_Task
                    (null, Target_Copy, null, Owner_1);
               end if;
            end;
         end;
      end;
   end Do_Swap_Tasks;

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
              " and before" & Job.Next'Img,
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

      --  Remove from bags
      Remove_From_All_Bags (This, Job);

      This.Contexts.Delete (Task_Key (Job.Job));

      Job := null;

      declare
         Cost  : Costs;
      begin
         pragma Remove_Expensive_Check;
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
         return This.MinMax.Last_Element.Cost;
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
               end if;
            end;
         elsif X in Agent_Context then
            null;
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
      return +This.Last_Mutation_Description;
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

      for I in M.First .. M.Last loop
         if Luck <= M.Vector (I).Prob then
            Log ("Performing mutation" & I'Img,
                 Debug, Section => Detail_Section);
            Reset (This.Last_Mutation_Undo);
            This.Last_Mutation_Index  := I;
            This.Last_Mutation_Exists := True;
            This.Last_Mutation_Undo.Was_Valid := This.Valid;

            --  This.Debug_Dump_Contexts;

            M.Vector (I).Doer (This,
                               This.Last_Mutation_Description,
                               This.Last_Mutation_Undo);

            Log ("Mutated: " & (+This.Last_Mutation_Description),
                 Debug, Section => Detail_Section);
            --  This.Debug_Dump_Contexts;
            --  Log ("** Minsum is " & To_String (This.Minsum), Always);
            if Expensive_Checks then
               declare
                  Old_Cost : constant Costs :=
                               This.Evaluate (This.Context.Ref.Criterion);
               begin
                  pragma Remove_This_Expensive_Check;
                  This.Reevaluate_Costs;
                  pragma Assert (Old_Cost = This.Evaluate (This.Context.Ref.Criterion));
               end;
            end if;
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
      Len : Natural;
      procedure Query (Key : Bag_Key;
                       Bag : Solution_Context_Bags.Object)
      is
         pragma Unreferenced (Key);
      begin
         Len := Bag.Length;
      end Query;
   begin
      Solution_Context_Bag_Maps.Query_Element
        (This.Bags.Find (All_Assigned_Tasks),
         Query'Access);
      return Len;
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
      C     : Cr.Cost_Matrix.Object renames This.Context.Ref.Costs;
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
                     use Cost_Matrix;
                     use Htn.Tasks;
                     T : Task_Context renames Task_Context (X);
                  begin
                     --  The case when Prev = No_Task is contemplated in the
                     --  Cost_Matrix object
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
      Reevaluate_Minsum (This, This.Minsum);

      This.MinMax.Clear;
      This.Agent_Costs.Clear;
      Agent_Maps.Iterate (This.Context.Ref.Agents, Ev'Access);

      This.Valid := This.MinSum < Infinite;
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
      Dummy_Desc : Ustring;
      Dummy_Undo : Undo_Info;
      C          : Static_Context_Access renames This.Context.Ref;
   begin
      C.Agents.Delete (Name);

      if This.Context.Ref.Agents.Is_Empty then
         Log ("Remove_Agent: No remaining agents!", Warning);
      else
         Reassign_Tasks
           (This, Name, Agent_Maps.First_Element (C.Agents).Get_Name);
         Do_Heuristic_1 (This, Dummy_Desc, Dummy_Undo);
      end if;
   end Remove_Agent;

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
      Empty_Undo : Undo_Info;
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

         --  Create task contexts
         while Has_Element (J) loop
            declare
               C : aliased Task_Context;
            begin
               C.Job := Element (J).Get_Id;
               if Has_Element (Previous (J)) then
                  C.Prev := Element (Previous (J)).Get_Id;
               end if;
               if Has_Element (Next (J)) then
                  C.Next := Element (Next (J)).Get_Id;
               end if;
               Set_Attribute (C'Access, Owner, Cr.Agent.Get_Name (A));

               --  Add to all tasks
               This.Add_To_Bag (C, All_Assigned_Tasks);
               --  Add to its agent
               This.Add_To_Bag (C, Agent_Tasks_Bag (A.Get_Name));

               This.Contexts.Insert
                 (Solution_Context_Key (Task_Key (C.Job)), C);

               Next (J);
            end;
         end loop;
      end Process_Agent;

      procedure Remove_Agent_Tasks (I : Cr.Agent.Containers.Lists.Cursor) is
         use Cr.Agent.Containers.Lists;
         use Htn.Tasks.Containers.Lists;
         A : constant Cr.Agent.Object'Class  := Element (I);
         T : constant Htn.Tasks.Containers.Lists.List   := A.Get_Tasks;
         J :          Htn.Tasks.Containers.Lists.Cursor := First (T);
      begin
         while Has_Element (J) loop
            Pending_Tasks.Delete (Element (J).Get_Id);
            Next (J);
         end loop;
      end Remove_Agent_Tasks;


   begin
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
            --  Cr.Cost_Matrix.Print (This.Context.Ref.Costs);
            Tasks.Insertions.Greedy
              (New_Ass,
               Htn.Tasks.Maps.First_Element (Pending_Tasks),
               This.Context.Ref.Costs,
               Criterion,
               New_New_Ass,
               Success);

            if not Success then
               Log ("Set_Assignment: cannot assign task " &
                    To_String (Integer (Pending_Tasks.First_Element.Get_Id)) &
                    "-" & Pending_Tasks.First_Element.To_String, Error,
                    Log_Section);
               raise Constraint_Error;
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
                        Costs : in     Cr.Cost_Matrix.Object)
   is
      C : constant Static_Context_Access := This.Context.Ref;
   begin
      C.Costs := Costs;
      Reevaluate_Costs (This);
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
         This.Valid := This.Last_Mutation_Undo.Was_Valid;
         Reset (This.Last_Mutation_Undo);
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
      Set_Assignment (This, Undo.Ass, This.Context.Ref.Criterion);
   end Undo_From_Scratch;

   -------------------
   -- Undo_Identity --
   -------------------

   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info) is
      pragma Unreferenced (This, Undo);
   begin
      null;
   end Undo_Identity;

   --------------------
   -- Undo_Move_Task --
   --------------------

   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info) is
      U : Undo_Info := Undo;
   begin
      while not U.Move_Stack.Is_Empty loop
         declare
            Move : Undo_Move_Task_Info renames
              U.Move_Stack.Vector (U.Move_Stack.Last);
            Src  : Task_Context_Ptr := This.Get_Task_Context (Move.Moved_One);
         begin
            This.Do_Move_Task
              (After_This  => This.Get_Task_Context (Move.Was_After),
               Src         => Src,
               Before_This => This.Get_Task_Context (Move.Was_Before),
               New_Owner   => Agent_Id (+Move.Owner_Was));

            if Move.Minsum_Was /= This.Minsum then
               Log ("Cost was " & To_String (Move.Minsum_Was) &
                    " but is " & To_String (This.Minsum), Error, Log_Section);
               raise Program_Error with "Undo breached cost integrity!";
            end if;
         end;
         U.Move_Stack.Delete (U.Move_Stack.Last);
      end loop;
   end Undo_Move_Task;

   function Proper_Cost (This : in Costs) return Costs;
   pragma Inline (Proper_Cost);

   function Proper_Cost (This : in Costs) return Costs is
   begin
      if This = Infinite then
         return Cost_For_Invalid_Task;
      else
         return This;
      end if;
   end Proper_Cost;
   --  If a cost is infinite, turn it into zero.
   --  Necessary for incremental evaluations of solutions since going to
   --  Costs'Last will cause loss of the known cost.

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
      Cm       : Cost_Matrix.Object renames This.Context.Ref.Costs;
      Pr, Ne   : Htn.Tasks.Task_Id := No_Task;
   begin

      if Prev /= null then
         Pr := Prev.Job;
      end if;
      if Next /= null then
         Ne := Next.Job;
      end if;

      Plus_1 := Cost_Matrix.Get_Cost (Cm,
                                      New_Owner,
                                      Pr,
                                      Curr);

      if Next /= null then
         Plus_2 := Cost_Matrix.Get_Cost (Cm,
                                         New_Owner,
                                         Curr,
                                         Ne);

         Minus   := Cost_Matrix.Get_Cost (Cm,
                                          New_Owner,
                                          Pr,
                                          Ne);
      end if;

      return Cr.Evaluate (This.Context.Ref.Criterion,
                          Minmax => Cost + Plus_1 + Plus_2 - Minus,
                          Minsum =>        Plus_1 + Plus_2 - Minus);
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
      Cm       : Cost_Matrix.Object renames This.Context.Ref.Costs;

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

      Plus_1 := Cost_Matrix.Get_Cost (Cm,
                                      New_Owner,
                                      Pr,
                                      Curr.Job);

      if Next /= null then
         Plus_2 := Cost_Matrix.Get_Cost (Cm,
                                         New_Owner,
                                         Curr.Job,
                                         Ne);

         Minus   := Cost_Matrix.Get_Cost (Cm,
                                          New_Owner,
                                          Pr,
                                          Ne);
      end if;

      if Plus_1 = Infinite then
         Plus_1 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Plus_2 = Infinite then
         Plus_2 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Minus = Infinite then
         Minus := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;

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
      Cm       : Cost_Matrix.Object renames This.Context.Ref.Costs;

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


      if Minus_1 = Infinite then
         Minus_1 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Minus_2 = Infinite then
         Minus_2 := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;
      if Plus = Infinite then
         Plus := Cost_For_Invalid_Task;
         This.Valid := False;
      end if;

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

end Agpl.Cr.Mutable_Assignment;
