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

with Ada.Numerics.Elementary_Functions;

package body Agpl.Optimization.Annealing is

   --------------------
   -- Lineal_Cooling --
   --------------------

   function Lineal_Cooling (T : in Temperature) return Temperature is
   begin
      return T - Temperature (1.0 / Float (Steps));
   exception
      when Constraint_Error =>
         if Cyclic then
            return Temperature'Last;
         else
            return Temperature'First; -- Exceeded iterations.
         end if;
   end Lineal_Cooling;

   --------------------------
   -- Proportional_Cooling --
   --------------------------

   function Proportional_Cooling (T : in Temperature) return Temperature is
   begin
      if Cyclic and then T < Umbral then
         return Temperature'Last;
      end if;

      return T * Temperature (Factor);
   end Proportional_Cooling;

   --------------------
   -- Cyclic_Cooling --
   --------------------

   function Cyclic_Cooling (T : in Temperature) return Temperature is
      pragma Unreferenced (T);
      use Ada.Calendar;
      use Ada.Numerics.Elementary_Functions;

      Elapsed : constant Float := Float ((Clock - Start) / Period);
      Remaind : constant Float := 1.0 - (Elapsed - Float'Floor (Elapsed));
   begin
      return Temperature (Remaind ** Power);
   end Cyclic_Cooling;
   --  T := ((Clock - Start) / Period) ^ Power
   --  Note that Start is reset if Clock - Start > Period

end Agpl.Optimization.Annealing;
