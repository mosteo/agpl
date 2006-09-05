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

with Agpl.Htn.Tasks.Containers;

package Agpl.Htn.Plan.Utils.Random is

   --  pragma Elaborate_Body;

   Log_Section : constant String := "agpl.htn.plan.utils";

   function Get_Any_Expansion (This : in Plan.Object;
                               Jobs : in Tasks.Containers.Lists.List)
                               return    Tasks.Containers.Lists.List;
   --  Given a list of tasks, and a plan with some methods for expansion,
   --  will return the tasks in some arbitrary expansion.
   --  Tasks are expanded one by one so no exponential problem can occur
   --  with OR expansions.
   --  May raise constraint error if some task fails to expand

   function Get_Any_Expansion (This : in Plan.Object) return Plan.Object;
   --  Given a plan with OR nodes, return a random plan expansion.
   --  This is truly random and memory efficient, so don't worry about the
   --  number of OR nodes

end Agpl.Htn.Plan.Utils.Random;
