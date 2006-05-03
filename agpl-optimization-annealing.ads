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

with Ada.Calendar;

--  OO implementation of the simulated annealing method

package Agpl.Optimization.Annealing is

   pragma Elaborate_Body;

   Log_Section    : constant String := "annealing";
   Detail_Section : constant String := "annealing.detail";

   type Temperature is new Float range 0.0 .. 1.0;
   --  Valid temperature ranges

   type Cost is new Float;
   --  We are *minimizing* the solution cost.
   --  If you want ot maximize an utility function, simply negate your function.

   type Acceptability is new Float range 0.0 .. 1.0;
   --  Probability of keeping a new solution

   subtype Float_0_1 is Float range 0.0 .. 1.0;

   generic
      Steps : Positive;
      Cyclic : Boolean := True;
   function Lineal_Cooling (T : in Temperature) return Temperature;
   --  (T := T - 1/Steps).
   --  If cyclic, Temp goes up to 1.0 if 0.0 is reached

   generic
      Factor : Float_0_1;
      Cyclic : Boolean     := True;
      Umbral : Temperature := 0.1; -- Point of reheating
   function Proportional_Cooling (T : in Temperature) return Temperature;
   --  (T := T * Factor)

   generic
      Start  : Ada.Calendar.Time;
      Period : Duration := 10.0;
      Power  : Float    := 1.0;
   function Cyclic_Cooling (T : in Temperature) return Temperature;
   --  T := ((Clock - Start) / Period) ^ Power
   --  Note that Start is reset if Clock - Start > Period

end Agpl.Optimization.Annealing;