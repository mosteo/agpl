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

--  An assigner creates assignments. Ideally it should aim to achieve some kind
--  of optimality.

with Agpl.Cr.Agent.Containers;
with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Matrix;
with Agpl.Htn.Plan;

package Agpl.Cr.Plan_Assigner is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   function Assign
     (This      : in Object;
      Agents    : in Agent.Containers.Vectors.Vector;
      Plan      : in Agpl.Htn.Plan.Object;
      Costs     : in Cost_Matrix.Object;
      Criterion : in Assignment_Criteria)
      return      Assignment.Object is abstract;
   --  Costs should include the starting task.

end Agpl.Cr.Plan_Assigner;
