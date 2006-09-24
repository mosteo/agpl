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

with Agpl.Cr.Cost_Cache;

package Agpl.Cr.Assigner.Greedy_Best_Pair_Tail is

   --  Greedy heuristic that at each step will select the pair agent-task which
   --  best fits the criterion.
   --  The new task for an agent will be tried just at end of plan.

   --  O (T * A * T) ~ O (n^3)

--  pragma Preelaborate;

   type Object is new Assigner.Object with record
      Criterion : Assignment_Criteria := Criterion_Time_Critical;
   end record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List;
      Costs  : in Cr.Cost_Cache.Object'Class)
      return      Assignment.Object;

end Agpl.Cr.Assigner.Greedy_Best_Pair_Tail;
