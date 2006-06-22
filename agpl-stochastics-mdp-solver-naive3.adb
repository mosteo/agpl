with Agpl.Command_Line;
with Agpl.Stochastics.Mdp.Bellman;
with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Solver.Common;

with Text_Io;

package body Agpl.Stochastics.Mdp.Solver.Naive3 is

   package CSO renames Containers.State_Outcomes_Maps;

   Stderr : Text_Io.File_Type renames Text_Io.Standard_Error;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Pr : in     Problem.Object;
      E  : in     Evolution_Function;
      T  : in     Transition_Function;
      R  : in     Reward_Function;
      A  : in     Action_Function;
      AE : in     Action_Evolution_Function;
      V  : in out Value_Function.Object;
      It : in     Positive := Positive'Last)
   is
      Reachable  : State.Object_Lists.List;
      Outcomes   : Containers.State_Outcomes_Maps.Map;
      Iterations : Natural := 0;
   begin
      --  Obtain reachable states
      Common.Reachable_States (Problem.Get_Initial_States (Pr), E, Reachable);
      Text_Io.Put_Line (Stderr, "# Reachable states:" &
                        Natural'Image (Natural (State.Object_Lists.Length
                                                  (Reachable))));

      --  Construct the outcome list
      Common.Appliable_Outcomes
        (Reachable,
         A, AE,
         Outcomes);
      Text_Io.Put_Line (Stderr, "Created outcome table");

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
               Result := Bellman.Pruned_Operator
                 (Element (S),
                  CSO.Element (CSO.Find (Outcomes,
                                         State.Get_Id (Element (S)))),
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

            if Command_Line.Exists ("--summary-value") then
               Value_Function.Summary (Vi);
            end if;

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

end Agpl.Stochastics.Mdp.Solver.Naive3;
