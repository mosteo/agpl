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

with Agpl.Generic_Handle;

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

--  OO implementation of the simulated annealing method

generic
   type Solution (<>) is private;
   --  An opaque type containing a solution.

   with function Evaluate (Sol : in Solution) return Cost;
   --  Says how good is a solution.

   with function Mutate (Sol : in Solution) return Solution;
   --  Creates a new solution from another one.

   with function Normalize (Old_Cost,
                            New_Cost : in Cost;
                            Temp     : in Temperature) return Acceptability;
   --  Say the probability of keeping a new solution, given the change in
   --  costs and current temperature.

   with function Last_Mutation (Sol : in Solution) return String;
   --  Informative, to know mutations working well
   --  Just returns a description of what was done.

   with function Is_Valid (Sol : in Solution) return Boolean;
   --  Debug, to know how many wasted mutations are generated
package Agpl.Optimization.Annealing.Solver is

   pragma Elaborate_Body;

   type Object is tagged limited private;
   --  The object used to perform the annealing

   function Best_Cost (This : in Object) return Cost;

   function Best_Solution (This : in Object) return Solution;
   --  Obtain the best solution seen till moment.

   function Current_Cost (This : in Object) return Cost;

   function Current_Solution (This : in Object) return Solution;

   function Current_Temperature (This : in Object) return Temperature;

   procedure Iterate (This   : in out Object;
                      Anneal : not null access function
                        (T : in Temperature) return Temperature);
   --  Do an iteration, and change the temperature. See parent package for
   --  some temperature change predefined functions.

   procedure Set_Initial_Solution (This : in out Object;
                                   Sol  : in     Solution);
   --  Starting solution

   procedure Set_Best_Solution (This : in out Object;
                                Sol  : in     Solution);
   --  If by some reason you alter it and need to replace...

   procedure Set_Current_Solution (This : in out Object;
                                   Sol  : in     Solution);
   --  Set the solution to be used as seed in the next iteration

   procedure Solve (This       : in out Object;
                    Ini_Sol    : in     Solution;
                    Anneal     : not null access function
                      (T : in Temperature) return Temperature;
                    Iterations : in     Positive;
                    Timeout    : in     Duration;
                    Converge   : in     Duration;
                    Progress   : access procedure
                      (Continue : out Boolean) := null);
   --  Run until Timeout expires or Converge time elapses without a better
   --  solution found or Iterations are performed.
   --  Callback is called once every second, just in case you want to do smthing

private

   package Sol_Handle is new Generic_Handle (Solution);

   type Object is tagged limited record
      Best_Sol   : Sol_Handle.Object;
      Curr_Sol   : Sol_Handle.Object;

      Best_Cost  : Cost := Cost'Last;
      Curr_Cost  : Cost;

      Curr_Temp  : Temperature := Temperature'Last;

      Random_Gen : Generator; -- Here, to make things reproducible.

      Iterations : Natural := 0; -- Total iterations run
      Discarded  : Natural := 0; -- Discarded moves
      Wasted     : Natural := 0; -- Invalid mutations seen
   end record;

end Agpl.Optimization.Annealing.Solver;
