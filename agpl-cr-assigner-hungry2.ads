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

with Agpl.Cr.Cost_Matrix;

package Agpl.Cr.Assigner.Hungry2 is

   pragma Preelaborate;

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return      Assignment.Object;
   --  Greedy heuristic that will get the agent with the minor cost for its
   --  least costly task.

end Agpl.Cr.Assigner.Hungry2;
