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

--  A generic interface for cost caching strategies.

with Agpl.Cr.Agent;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Containers;

package Agpl.Cr.Cost_Cache is

   pragma Preelaborate;

   --  type Object is interface;
   type Object is abstract tagged null record;

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Htn.Tasks.Task_Id;
      Fin   : in Htn.Tasks.Task_Id) return Costs is abstract;

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in Cr.Agent.Object'Class) return Costs;
   --  Say the full cost of an agent plan.

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in String;
      Tasks : in Htn.Tasks.Containers.Lists.List) return Costs;
   --  Evaluate a plan with a given agent

end Agpl.Cr.Cost_Cache;
