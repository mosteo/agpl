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

--  This assigner will chose in every step the best pairing amongst all tasks
--  in all expansions.

package Agpl.Cr.Plan_Assigner.Greedy1 is

   --  pragma Preelaborate;

   Log_Section : constant String := "agpl.cr.plan_assigner.greedy1";

   type Object is new Plan_Assigner.Object with null record;

   function Assign
     (This      : in Object;
      Agents    : in Agent.Containers.Vectors.Vector;
      Plan      : in Agpl.Htn.Plan.Object;
      Costs     : in Cost_Matrix.Object;
      Criterion : in Assignment_Criteria)
      return      Assignment.Object;
   --  Costs should include the starting task.

end Agpl.Cr.Plan_Assigner.Greedy1;
