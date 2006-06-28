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

package Agpl.Optimization.Mtsp is

   pragma Preelaborate;

   No_Solution : exception;

   type Cost_Matrix is array (Positive range <>,
                              Positive range <>,
                              Positive range <>) of Float;
   --  First index is the salesman index.
   --  Second and third is city index.
   --  Value should be float'last if not allowed, 0 for the same city.

   type Result_Matrix is array (Positive range <>,
                                Positive range <>) of Natural;
   --  First index is the salesman index.
   --  Second index is the city index.
   --  The value says in which stage the city is to be visited by the salesman.
   --  It will be zero for non-visited cities.

   type Start_Matrix is array (Positive range <>) of Positive;
   --  Index is the salesman index.
   --  Value is in which city it starts.

   type Stage_Matrix is array (Positive range <>) of Natural;
   --  Index is salesman.
   --  Value is how many cities has already visited.

   subtype Pos_Matrix is Start_Matrix;
   --  Index is salesman.
   --  Value is last visited city.

   function Brute_Force (Costs : in Cost_Matrix) return Result_Matrix;
   --  Tries all combinations and return the best assignation.
   --  All salesmen start at city 1.
   --  May raise No_Solution.

   function Get_Total_Cost (Costs : in Cost_Matrix;
                            Sol   : in Result_Matrix) return Float;
   --  Total cost incurred by all salesmen.

   function Get_Max_Min_Cost (Costs : in Cost_Matrix;
                              Sol   : in Result_Matrix) return Float;
   --  Cost of the worse salesman.

end Agpl.Optimization.Mtsp;
