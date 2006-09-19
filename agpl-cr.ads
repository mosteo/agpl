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

--  Root package for Cooperative Robotics

--  with Agpl.Optimization;

package Agpl.Cr is

   pragma Preelaborate;

   type Costs is delta 0.01 digits 18;
   --  This *must* be a fixed point type, otherwise incremental cost computation
   --  in annealing will fail.

   use type Costs;

   Infinite : constant Costs := Costs'Last;

   type Assignment_Criteria is record
      Minmax_Weight : Float := 0.0;
      Minsum_Weight : Float := 0.0;
   end record;
   --  Possibilities for assignments
   --  Defaults to invalid to ensure that we set it!

   function Evaluate (Criterion : in Assignment_Criteria;
                      Minmax    : in Costs;
                      Minsum    : in Costs) return Costs;
   pragma Inline (Evaluate);

   function Image (C : in Costs) return String;
   pragma Inline (Image);

   function Image (C : in Costs; Decimals : in Natural) return String;
   pragma Inline (Image);

   function Value (S : in String) return Assignment_Criteria;

   Criterion_Invalid       : constant Assignment_Criteria := (0.0, 0.0);
   Criterion_Minmax        : constant Assignment_Criteria := (1.0, 0.0);
   Criterion_Minsum        : constant Assignment_Criteria := (0.0, 1.0);
   Criterion_Minmix        : constant Assignment_Criteria := (1.0, 1.0);
   Criterion_Mintim        : constant Assignment_Criteria := (1.0, 0.00001);

   Criterion_Minimax       : Assignment_Criteria renames Criterion_Minmax;
   Criterion_Totalsum      : Assignment_Criteria renames Criterion_Minsum;
   Criterion_Time_Critical : Assignment_Criteria renames Criterion_Minmix;
   Criterion_Best          : Assignment_Criteria renames Criterion_Minmix;

end Agpl.Cr;
