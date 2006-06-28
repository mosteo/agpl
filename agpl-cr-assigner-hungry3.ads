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

package Agpl.Cr.Assigner.Hungry3 is

   type Object (Keep_Order : Boolean) is new Assigner.Object with null record;
   --  If Keep_Order, any tasks in an Agent passed to Agents will be kept in
   --  that position (at plan start).
   --  If Keep_Order is false, all tasks will be planned in equal conditions.

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return      Assignment.Object;

   --  Greedy heuristic that at each step will select the pair agent-task which
   --  adds less cost to the minimax cost.

end Agpl.Cr.Assigner.Hungry3;
