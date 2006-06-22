with Agpl.Stochastics.Mdp.Bellman;
with Agpl.Stochastics.Mdp.Solver.Common;

with Text_Io;

package body Agpl.Stochastics.Mdp.Solver.Naive is

   Stderr : Text_Io.File_Type renames Text_Io.Standard_Error;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Pr : in     Problem.Object;
      E  : in     Evolution_Function;
      T  : in     Transition_Function;
      R  : in     Reward_Function;
      V  : in out Value_Function.Object;
      It : in     Positive := Positive'Last)
   is
      Reachable  : State.Object_Lists.List;
      Actions    : Action.Object_Lists.List := Problem.Get_Actions (Pr);
      Iterations : Natural := 0;
   begin
      --  Obtain reachable states
      Common.Reachable_States (Problem.Get_Initial_States (Pr), E, Reachable);
      Text_Io.Put_Line (Stderr, "# Reachable states:" &
                        Natural'Image (Natural (State.Object_Lists.Length
                                                  (Reachable))));

      --  Perform value iteration over reachable states
      loop
         declare
            use State.Object_Lists;
            S : State.Object_Lists.Cursor := State.Object_Lists.First (Reachable);

            Vi : Value_Function.Object;
            use type Value_Function.Object;

            Result : Bellman.Result;
         begin
            while S /= No_Element loop
               Result := Bellman.Operator
                 (Element (S),
                  Actions,
                  Reachable,
                  V,
                  T,
                  R,
                  Problem.Get_Discount (Pr));

               Value_Function.Set_Value
                 (Vi,
                  State.Object_Lists.Element (S),
                  Bellman.Get_Reward (Result),
                  Bellman.Get_Action (Result));

               Next (S);
            end loop;

            Iterations := Iterations + 1;
            exit when Vi = V;
            V := Vi;
         end;

         exit when Iterations = It;
         Text_Io.Put_Line (Stderr, "Iteration" & Iterations'Img);

      end loop;

      if Iterations = It then
         Text_Io.Put_Line (Stderr,
                           "Convergence not reached after" & It'Img &
                           " iterations");
      else
         Text_Io.Put_Line (Stderr,
                           "Converged in" & Iterations'Img &
                           " iterations");
      end if;

   end Solve;

end Agpl.Stochastics.Mdp.Solver.Naive;
