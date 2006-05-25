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
      null;
   end Adjust;

   -------------------
   -- Do_Flip_Worst --
   -------------------

   procedure Do_Flip_Worst   (This : in out Object;
                              Desc :    out Ustring;
                              Undo :    out Undo_Info)
   is
   begin
      null;
   end Do_Flip_Worst;

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
      Undo := Undo_Info'(null record);
   end Do_Identity;

   ----------------------
   -- Evaluate_Minimax --
   ----------------------

   function Evaluate_Minimax (This : in Object) return Cost is
   begin
      return This.Minimax.Last_Element.Cost;
   end Evaluate_Minimax;

   -----------------------
   -- Evaluate_Totalsum --
   -----------------------

   function Evaluate_Totalsum (This : in Object) return Cost is
   begin
      return This.Totalsum;
   end Evaluate_Totalsum;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
   begin
      This.Context.Bind (new Static_Context);
   end Initialize;

   -------------------
   -- Last_Mutation --
   -------------------

   function Last_Mutation (This : in Object) return String is
   begin
      return +This.Last_Mutation_Description;
   end Last_Mutation;

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

   procedure Remove_Agent (This : in out Object; Name : in String) is
   begin
      null;
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

   -------------------
   -- Undo_Identity --
   -------------------

   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info) is
      pragma Unreferenced (This, Undo);
   begin
      null;
   end Undo_Identity;

end Agpl.Cr.Mutable_Assignment;
