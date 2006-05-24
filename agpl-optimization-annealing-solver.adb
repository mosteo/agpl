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

with Agpl.Chronos;
with Agpl.Cr;
with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Optimization.Annealing.Solver is

   use type Cr.Costs;

   ---------------
   -- Best_Cost --
   ---------------

   function Best_Cost (This : in Object) return Cost is
   begin
      return This.Best_Cost;
   end Best_Cost;

   -------------------
   -- Best_Solution --
   -------------------

   function Best_Solution (This : in Object) return Solution is
   begin
      return This.Best_Sol.Get;
   end Best_Solution;

   ------------------
   -- Current_Cost --
   ------------------

   function Current_Cost (This : in Object) return Cost is
   begin
      return This.Curr_Cost;
   end Current_Cost;

   ----------------------
   -- Current_Solution --
   ----------------------

   function Current_Solution (This : in Object) return Solution is
   begin
      return This.Curr_Sol.Get;
   end Current_Solution;

   -------------------------
   -- Current_Temperature --
   -------------------------

   function Current_Temperature (This : in Object) return Temperature is
   begin
      return This.Curr_Temp;
   end Current_Temperature;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (This   : in out Object;
                      Anneal : not null access function
                        (T : in Temperature) return Temperature)
   is
   begin
      declare
         New_Cost :          Cost;
         P        : constant Float := Random (This.Random_Gen);
         Goodness :          Float;
      begin
         Mutate (This.Curr_Sol.Ref.all); -- Mutate in place
         New_Cost := Evaluate (This.Curr_Sol.Ref.all);
         Goodness := Float (Normalize
                              (Old_Cost => This.Curr_Cost,
                               New_Cost => New_Cost,
                               Temp     => This.Curr_Temp));

         --         Log ("Move: " & Last_Mutation (New_Sol), Always);
         This.Iterations := This.Iterations + 1;

         if New_Cost = Infinite then -- Invalid solution
            This.Wasted := This.Wasted + 1;
            Undo (This.Curr_Sol.Ref.all);
         elsif New_Cost < This.Best_Cost or else P < Goodness then
            --  See if we must replace the current solution
            --              Log ("New Sol accepted with " &
            --                   Strings.To_String (P) & " < " &
            --                   Strings.To_String (Goodness), Always);

            --  Keep best solution seen:
            if New_Cost < This.Best_Cost then
               This.Best_Cost := New_Cost;
               This.Best_Sol.Set (This.Curr_Sol.Get);
            end if;

            This.Curr_Cost := New_Cost;
         else
            This.Discarded := This.Discarded + 1;
            Undo (This.Curr_Sol.Ref.all);
         end if;

         This.Curr_Temp := Anneal (This.Curr_Temp);
      end;
   exception
      when E : others =>
         Log ("Annealing.Solver.Iterate: " & Report (E), Error);
         raise;
   end Iterate;

   --------------------------
   -- Set_Initial_Solution --
   --------------------------

   procedure Set_Initial_Solution (This : in out Object;
                                   Sol  : in     Solution)
   is
   begin
      This.Best_Sol.Set (Sol);
      This.Curr_Sol.Set (Sol);

      This.Best_Cost := Evaluate (Sol);
      This.Curr_Cost := This.Best_Cost;

      This.Curr_Temp := Temperature'Last;

      This.Discarded  := 0;
      This.Iterations := 0;
      This.Wasted     := 0;
   end Set_Initial_Solution;

   -----------------------
   -- Set_Best_Solution --
   -----------------------

   procedure Set_Best_Solution (This : in out Object;
                                Sol  : in     Solution)
   is
   begin
      This.Best_Sol.Set (Sol);
      This.Best_Cost := Evaluate (Sol);
   end Set_Best_Solution;

   procedure Set_Current_Solution (This : in out Object;
                                   Sol  : in     Solution)
   is
   begin
      This.Curr_Sol.Set (Sol);
      This.Curr_Cost := Evaluate (Sol);
   end Set_Current_Solution;

   -----------
   -- Solve --
   -----------

   procedure Solve (This       : in out Object;
                    Ini_Sol    : in     Solution;
                    Anneal     : not null access function
                      (T : in Temperature) return Temperature;
                    Iterations : in     Positive;
                    Timeout    : in     Duration;
                    Converge   : in     Duration;
                    Progress   : access procedure (Continue : out Boolean) := null)
   is
      use Chronos;
      Global_Timer,
      Converge_Timer,
      Info_Timer     : Chronos.Object;

      Best_Cost      : Cost;
      Remaining_Iter : Natural := Iterations;
      Continue       : Boolean := True;
   begin
      This.Set_Initial_Solution (Ini_Sol);
      Best_Cost := This.Best_Cost;

      Log ("(Startup) Initial solution cost is " &
                    Strings.To_String (Float (Best_Cost)), Debug,
                    Section => Log_Section);

      while Continue and then
            Remaining_Iter > 0 and then
            Elapsed (Converge_Timer) < Converge and then
            Elapsed (Global_Timer) < Timeout
      loop
         This.Iterate (Anneal);

         Log ("Iteration:" & This.Iterations'Img,
              Debug, Section => Detail_Section);

         --  Check for progress...
         declare
            Curr_Best : constant Cost := This.Best_Cost;
         begin
            if Curr_Best < Best_Cost then
               Best_Cost := Curr_Best;
               Converge_Timer.Reset;
               Log ("(Iteration" & This.Iterations'Img &
                    " at " & Image (Global_Timer) & ") " &
                    "(speed: " & Strings.To_String
                      (Float (This.Iterations) / Float (Elapsed (Global_Timer))) &
                    " i/s)" &" Best solution: " &
                    Strings.To_String (Float (Best_Cost)) &
                    " obtained via " & Last_Mutation (This.Best_Solution),
                    Debug,
                    Section => Log_Section);
            end if;
         end;

         Remaining_Iter := Remaining_Iter - 1;

         if Elapsed (Info_Timer) >= 1.0 then
            Reset (Info_Timer);

            if Progress /= null then
               Progress.all (Continue);
            end if;

--              Log ("Iteration" & This.Iterations'Img &
--                   "; Curr_Cost: " & Strings.To_String (Float (Current_Cost (This))) &
--                   "; Curr_Temp: " & Strings.To_String (Float (Current_Temperature (This))) &
--                   "; Curr_Move: " & Last_Mutation (Current_Solution (This)),
--                   Debug, Section => Log_Section);
         end if;
      end loop;

      Log ("Best cost found: " & Strings.To_String (Float (Best_Cost)) &
           " in" & This.Iterations'Img & " iterations run (" &
           Image (Global_Timer) & ", " &
           Strings.To_String
             (Float (This.Iterations) / Float (Elapsed (Global_Timer))) &
           " i/s) (" &
           Integer'Image (This.Wasted * 100 / This.Iterations) & "% wasted moves)" &
           " (" & Integer'Image (This.Discarded * 100 / This.Iterations) & "% discarded moves)",
           Debug, Section => Log_Section);

      if Remaining_Iter = 0 then
         Log ("Annealing cycles exhausted", Debug, Section => Log_Section);
      elsif Elapsed (Converge_Timer) >= Converge then
         Log ("No progress found in convergence period",
              Debug, Section => Log_Section);
      elsif Elapsed (Global_Timer) >= Timeout then
         Log ("Annealing time exhausted", Debug, Section => Log_Section);
      end if;

   end Solve;

end Agpl.Optimization.Annealing.Solver;
