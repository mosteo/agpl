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
with Agpl.Cr.Assigner.Hungry3;
with Agpl.Htn.Plan_Node;
with Agpl.Random;
with Agpl.Trace;   use Agpl.Trace;

with Ada.Unchecked_Deallocation;

package body Agpl.Cr.Mutable_Assignment is

   use type Optimization.Cost;
   use type Optimization.Annealing.Probability;

   function S is new Conversions.To_Str (Optimization.Annealing.Probability);

   function "<" (L, R : Minimax_Key) return Boolean is
      use Asu;
      use Optimization.Annealing;
   begin
      return
        L.Cost < R.Cost or else (L.Cost = R.Cost and then L.Agent < R.Agent);
   end "<";

   procedure Free is new Ada.Unchecked_Deallocation (Solution_Context'Class,
                                                     Solution_Context_Access);

   ---------------
   -- Add_Agent --
   ---------------

   procedure Add_Agent (This : in out Object; Name : in Agent_Id) is
   begin
      This.Context.Ref.Agents.Include (Name);
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
         Context.Bag_Indexes.Insert (Key, Solution_Context_Bags.Last (Bag));
      end Add;
   begin
      Solution_Context_Bag_Maps.Update_Element
        (This.Bags.Find (Bag), Add'Access);
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
      use Solution_Context_Maps;
      procedure Free (I : Cursor) is
         X : Solution_Context_Access := Element (I);
      begin
         Free (X);
      end Free;
   begin
      Iterate (This.Contexts, Free'Access);
      This.Contexts.Clear;
      This.Bags.Clear;
   end Clear_Dynamic_Part;

   ----------------------
   -- Do_Heuristic_All --
   ----------------------

   procedure Do_Heuristic_All (This : in out Object;
                               Desc :    out Ustring;
                               Undo :    out Undo_Info)
   is
      A : Cr.Assignment.Object := This.To_Assignment;
   begin
      Undo.Scratch.Set (This);
      Desc := +"Heuristic - All";
      declare
         use Cr.Assignment;
         New_Assignment : constant Cr.Assignment.Object :=
                            Cr.Assigner.Hungry3.Assign
                              ((Cr.Assigner.Object with Keep_Order => True),
                               Get_Agents_Without_Tasks (A),
                               Get_All_Tasks (A),
                               This.Context.Ref.Costs);
      begin
         Set_Assignment (This, New_Assignment);
      end;
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
            Prev := Task_Context_Access (Element (P));
         end if;
         if Has_Element (N) then
            Next := Task_Context_Access (Element (N));
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

      --  Remove from bags
      Remove_From_All_Bags (This, Solution_Context_Access (Job));

      This.Contexts.Delete (Task_Key (Job.Job));
   end Do_Remove_Task;

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

   function Get_Attribute (This : in Solution_Context_Access;
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
      This.Context.Bind (new Static_Context);
   end Initialize;

   -------------
   -- Is_Sane --
   -------------

   function Is_Sane (This : in Object) return Boolean is

      procedure Check_Contexts (I : Solution_Context_Maps.Cursor) is
         X : constant Solution_Context_Access :=
               Solution_Context_Maps.Element (I);
      begin
         if X.all in Task_Context then
            declare
               Node : constant Htn.Plan.Subplan :=
                        Htn.Plan.Get_Node
                          (This.Context.Ref.Plan, Task_Context (X.all).Job);
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
        (Element (Context.Bag_Indexes.Find (Bag_Key (+Bag.Key))) = Prev);

      Context.Bag_Indexes.Delete (Bag_Key (+Bag.Key));
      Context.Bag_Indexes.Insert (Bag_Key (+Bag.Key), Curr);
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
            This.Last_Mutation_Index := I;
            M.Vector (I).Doer (This,
                               This.Last_Mutation_Description,
                               This.Last_Mutation_Undo);
            return;
         end if;
      end loop;
      Log ("Mutate: No mutation performed!", Error);
   end Mutate;

   --------------------
   -- Reassign_Tasks --
   --------------------

   procedure Reassign_Tasks (This : in out Object; From, To : in Agent_Id) is
      procedure It (I : in Solution_Context_Maps.Cursor) is
         X : constant Solution_Context_Access :=
               Solution_Context_Maps.Element (I);
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
         X    : constant Solution_Context_Access := Element (I);
         Curr :          Costs;
      begin
         if X.all in Task_Context then
            if Agent_Id (Get_Attribute (X, Owner)) = Agent then
               declare
                  use Cost_Matrix;
                  use Htn.Tasks;
                  T : Task_Context renames Task_Context (X.all);
               begin
                  --  If there's no previous, this is the Starting task so
                  --  it doesnt' count
                  if T.Prev /= No_Task then
                     Curr := Get_Cost (C, String (Agent), T.Prev, T.Job);
                     if Curr = Infinite then
                        Total := Infinite;
                     else
                        Total := Total + Curr;
                     end if;
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
   begin
      This.Totalsum := Reevaluate_Totalsum (This);
   end Reevaluate_Costs;

   ------------------------
   -- Reevaluate_Minimax --
   ------------------------

   function Reevaluate_Minimax    (This : in Object)
                                   return    Costs
   is
   begin
      return 0.0;
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
         Do_Heuristic_All (This, Dummy_Desc, Dummy_Undo);
      end if;
   end Remove_Agent;

   --------------------------
   -- Remove_From_All_Bags --
   --------------------------

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : in     Solution_Context_Access)
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
                              Context : in     Solution_Context_Access;
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
      Context : in     Solution_Context_Access;
      Bag     : in     Solution_Context_Bag_Maps.Cursor)
   is
      pragma Unreferenced (This);
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
      Solution_Context_Bag_Maps.Update_Element (Bag, Remove'Access);
   end Remove_From_Bag;

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
      C : constant Static_Context_Access := This.Context.Ref;
   begin
      C.Costs := Costs;
      Reevaluate_Costs (This);
   end Set_Costs;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute (This : in Solution_Context_Access;
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
   begin
      null;
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

   -----------------------
   -- Undo_From_Scratch --
   -----------------------

   procedure Undo_From_Scratch (This : in out Object;
                                Undo : in     Undo_Info)
   is
   begin
      This := Object (Undo.Scratch.Get);
      This.Last_Mutation_Undo.Scratch.Clear;
   end Undo_From_Scratch;

   ------------------------
   -- Undo_Heuristic_All --
   ------------------------

   procedure Undo_Heuristic_All (This : in out Object; Undo : in  Undo_Info) is
   begin
      Undo_From_Scratch (This, Undo);
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
