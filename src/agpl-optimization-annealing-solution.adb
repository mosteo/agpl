with Agpl.Conversions;
with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Optimization.Annealing.Solution is

   function S is new Conversions.To_Str (Optimization.Annealing.Probability);

   ------------------
   -- Add_Mutation --
   ------------------

   procedure Add_Mutation
     (This    : in out Object;
      Name    :        String;
      Mutator : not null Mutation_Doer;
      Undoer  : not null Mutation_Undoer;
      Weight  :        Float   := 1.0)
   is
   begin
      This.Mutations.Append
        ((Name   => +Name,
          Doer   => Mutator,
          Undoer => Undoer,
          Weight => Weight,
          Prob   => 0.0));

      --  Adjust new probabilities
      declare
         Acum_Weight  : Float := 0.0;
         Total_Weight : Float := 0.0;
         M            : Mutation_Vectors.Object renames This.Mutations;
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
                    Debug, Detail_Section);
            else
               Log ("Mutation" & I'Img & ": P = " &
                    S (M.Vector (I).Prob - M.Vector (I - 1).Prob, 5),
                    Debug, Detail_Section);
            end if;
         end loop;
      end;
   end Add_Mutation;

   -------------------
   -- Last_Mutation --
   -------------------

   function Last_Mutation
     (This : Object)
      return String
   is
   begin
      if +This.Last_Mutation_Name = "" then
         raise Constraint_Error with "No mutation yet";
      else
         return +This.Last_Mutation_Name;
      end if;
   end Last_Mutation;

   ------------
   -- Mutate --
   ------------

   procedure Mutate
     (This : in out Object)
   is
      use Optimization.Annealing;
      Luck : constant Probability := Probability (Random.Uniform);
      M    :          Mutation_Vectors.Object renames This.Mutations;
   begin
      for I in M.First .. M.Last loop
         if Luck <= M.Vector (I).Prob then

            Log ("Performing mutation " & (+M.Vector (I).Name),
                 Debug, Section => Detail_Section);

            This.Last_Mutation_Name := M.Vector (I).Name;
            This.Last_Mutation_Undo := M.Vector (I).Undoer;
            M.Vector (I).Doer (This);

            Log ("Mutated: " & (+This.Last_Mutation_Name),
                 Debug, Section => Detail_Section);

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

   ----------
   -- Undo --
   ----------

   procedure Undo
     (This : in out Object)
   is
   begin
      This.Last_Mutation_Undo (This);
   end Undo;

end Agpl.Optimization.Annealing.Solution;
