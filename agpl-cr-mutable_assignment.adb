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
with Agpl.Random;
with Agpl.Trace;   use Agpl.Trace;

package body Agpl.Cr.Mutable_Assignment is

   use type Optimization.Annealing.Probability;

   function S is new Conversions.To_Str (Optimization.Annealing.Probability);

   function "<" (L, R : Minimax_Key) return Boolean is
      use Asu;
      use Optimization.Annealing;
   begin
      return
        L.Cost < R.Cost or else (L.Cost = R.Cost and then L.Agent < R.Agent);
   end "<";

   ------------------
   -- Add_Mutation --
   ------------------

   procedure Add_Mutation (This    : in out Object;
                           Mutator : not null Mutation_Doer;
                           Undoer  : not null Mutation_Undoer;
                           Weight  : in     Float   := 1.0)
   is
   begin
      This.Context.Ref.Mutations.Append ((Doer   => Mutator,
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

   ------------
   -- Adjust --
   ------------

   procedure Adjust     (This : in out Object) is
   begin
      raise Program_Error;
   end Adjust;

   -------------------
   -- Do_Flip_Worst --
   -------------------

   procedure Do_Flip_Worst   (This : in out Object;
                              Desc :    out Ustring;
                              Undo :    out Undo_Info)
   is
   begin

      declare
         Worst_Name : constant String := + This.Minimax.Last_Element.Agent;
      begin
         null;
      end;
   end Do_Flip_Worst;

   ----------------------
   -- Do_Heuristic_All --
   ----------------------

   procedure Do_Heuristic_All (This : in out Object;
                               Desc :    out Ustring;
                               Undo :    out Undo_Info)
   is
   begin
      raise Program_Error;
   end Do_Heuristic_All;

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
      Undo := (Kind => Identity);
   end Do_Identity;

   --------------------------------
   -- Do_Temporarily_Remove_Task --
   --------------------------------
   --  O (ln A) because the agent bags must be lookd up.
   procedure Do_Temporarily_Remove_Task (This : in out Object;
                                         Job  : not null Task_Context_Access)
   is
      Or_Parent : Or_Node_Context_Access;

      procedure Remove_From_Agent_Or_Bag (Key : in     String;
                                          Bag : in out Or_Node_Bags.Object)
      is
         pragma Unreferenced (Key);
      begin
         Bag.Delete (Or_Parent.Idx_In_Agent_Bag,
                     Moving_Or_Context'Access);
      end Remove_From_Agent_Or_Bag;

      procedure Remove_From_Agent_Task_Bag (Key : in     String;
                                            Bag : in out Task_Bags.Object)
      is
         pragma Unreferenced (Key);
      begin
         Bag.Delete (Job.Owner_Bag_Idx,
                     Moving_Task_Context'Access);
      end Remove_From_Agent_Task_Bag;
   begin
      --  OR things
      if Job.Is_Flippable then
         Or_Parent :=
           Or_Node_Maps.Element (This.Nodes.Find (+Job.Or_Parent_Id));
         --  Remove from agent bag
         Or_Node_Bag_Maps.Update_Element (This.Agent_Nodes.Find (+Job.Owner),
                                          Remove_From_Agent_Or_Bag'Access);
      end if;

      --  Costs to be updated
      declare
         P, N       : Task_Maps.Cursor;
         Prev, Next : Task_Context_Access;
         use Task_Maps;
      begin
         P := This.Tasks_By_Id.Find (Job.Prev);
         N := This.Tasks_By_Id.Find (Job.Next);
         if Has_Element (P) then
            Prev := Element (P);
         end if;
         if Has_Element (N) then
            Next := Element (N);
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
         Job.Prev := Htn.Tasks.No_Task;
         Job.Next := Htn.Tasks.No_Task;
      end;

      --  Remove from task bags
      Task_Bag_Maps.Update_Element (This.Agent_Tasks.Find (+Job.Owner),
                                    Remove_From_Agent_Task_Bag'Access);
      Job.Owner := +No_Agent;

      This.Tasks_Bag.Delete (Job.General_Bag_Idx,
                             Moving_Task_Context'Access);
   end Do_Temporarily_Remove_Task;

   ----------------------
   -- Evaluate_Minimax --
   ----------------------

   function Evaluate_Minimax (This : in Object) return Cost is
   begin
      if This.Valid then
         return This.Minimax.Last_Element.Cost;
      else
         return Optimization.Annealing.Infinite;
      end if;
   end Evaluate_Minimax;

   -----------------------
   -- Evaluate_Totalsum --
   -----------------------

   function Evaluate_Totalsum (This : in Object) return Cost is
   begin
      if This.Valid then
         return This.Totalsum;
      else
         return Optimization.Annealing.Infinite;
      end if;
   end Evaluate_Totalsum;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
   begin
      This.Context.Bind (new Static_Context);

      This.Tasks_Bag.Set_Context (General_Tasks_Bag);
      This.Flip_Nodes.Set_Context (Flip_Or_Bag);
      This.Ready_Nodes.Set_Context (Ready_Or_Bag);
   end Initialize;

   -------------------
   -- Last_Mutation --
   -------------------

   function Last_Mutation (This : in Object) return String is
   begin
      return +This.Last_Mutation_Description;
   end Last_Mutation;

   -----------------------
   -- Moving_Or_Context --
   -----------------------

   procedure Moving_Or_Context (This    : in out Or_Node_Context_Access;
                                Context : in out Bag_Context;
                                Prev,
                                Curr    : in     Integer)
   is
   begin
      case Context is
         when Agent_OR_Bag =>
            pragma Assert (Prev = This.Idx_In_Agent_Bag);
            This.Idx_In_Agent_Bag := Curr;
         when Flip_Or_Bag =>
            pragma Assert (Prev = This.Idx_In_Flip_Bag);
            This.Idx_In_Flip_Bag := Curr;
         when Ready_Or_Bag =>
            pragma Assert (Prev = This.Idx_In_Nodes);
            This.Idx_In_Nodes := Curr;
         when others =>
            raise Program_Error;
      end case;
   end Moving_Or_Context;

   -------------------------
   -- Moving_Task_Context --
   -------------------------

   procedure Moving_Task_Context (This    : in out Task_Context_Access;
                                  Context : in out Bag_Context;
                                  Prev,
                                  Curr    : in     Integer)
   is
   begin
      case Context is
         when Agent_Tasks_Bag =>
            pragma Assert (Prev = This.Owner_Bag_Idx);
            This.Owner_Bag_Idx := Curr;
         when General_Tasks_Bag =>
            pragma Assert (Prev = This.General_Bag_Idx);
            This.General_Bag_Idx := Curr;
         when others =>
            raise Program_Error;
      end case;
   end Moving_Task_Context;

   ------------
   -- Mutate --
   ------------

   procedure Mutate (This : in out Object) is
      use Optimization.Annealing;
      Luck : constant Probability := Probability (Random.Uniform);
      M    :          Mutation_Vectors.Object renames
        This.Context.Ref.Mutations;
   begin
      for I in M.First .. M.Last loop
         if Luck <= M.Vector (I).Prob then
            Log ("Performing mutation" & I'Img,
                 Debug, Section => Detail_Section);
            This.Last_Mutation_Index := I;
            M.Vector (I).Doer (This,
                               This.Last_Mutation_Description,
                               This.Last_Mutation_Undo);
            return;
         end if;
      end loop;
      Log ("Mutate: No mutation performed!", Error);
   end Mutate;

   ------------------
   -- Remove_Agent --
   ------------------
   --  O (n) or worse (depending on the heuristic used).
   procedure Remove_Agent (This : in out Object; Name : in String) is
   begin
      --  If it was the last one, finalize the object
      if Natural (This.Agent_Tasks.Length) = 1 then
         Finalize (This);
      else
         --  Copy all tasks to any other agent and do some assignation.
      end if;
   end Remove_Agent;

   --------------------
   -- Set_Assignment --
   --------------------

   procedure Set_Assignment (This : in out Object;
                             Ass  : in     Cr.Assignment.Object)
   is
   begin
      null;
   end Set_Assignment;

   ---------------
   -- Set_Costs --
   ---------------

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Matrix.Object)
   is
   begin
      null;
   end Set_Costs;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object)
   is
   begin
      null;
   end Set_Tasks;

   -------------------
   -- To_Assignment --
   -------------------
   function To_Assignment   (This : in Object) return Cr.Assignment.Object is
      Result : Cr.Assignment.Object;
   begin
      return Result;
   end To_Assignment;

   ----------
   -- Undo --
   ----------

   procedure Undo (This : in out Object) is
   begin
      This.Context.Ref.Mutations.Vector
        (This.Last_Mutation_Index).Undoer (This, This.Last_Mutation_Undo);
   end Undo;

   ---------------------
   -- Undo_Flip_Worst --
   ---------------------

   procedure Undo_Flip_Worst (This : in out Object; Undo : in  Undo_Info) is
   begin
      null;
   end Undo_Flip_Worst;

   ------------------------
   -- Undo_Heuristic_All --
   ------------------------

   procedure Undo_Heuristic_All (This : in out Object; Undo : in  Undo_Info) is
   begin
      raise Program_Error;
   end Undo_Heuristic_All;

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
   begin
      null;
   end Update_Costs_Removing;

end Agpl.Cr.Mutable_Assignment;
