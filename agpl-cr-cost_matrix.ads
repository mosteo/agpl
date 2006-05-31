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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Agpl.Cr.Agent.Lists;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Lists;

package Agpl.Cr.Cost_Matrix is

   pragma Elaborate_Body;

   type Object is private;
   --  Here we store a mapping of Agent x Start Task x End Task --> Costs
   --  This structure will be later used by assigners to compute an assignation.

   function Add_Starting_Tasks
     (Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List) return Htn.Tasks.Lists.List;
   --  Get a copy of Tasks including Cr.Tasks.Starting_Task for each agent
   --  The starting tasks will be the first ones in the order in which agents are.

   procedure Create
     (This   : in out Object;
      Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List);
   --  Create a matrix given a list of agents and tasks to perform.
   --  O (|A||T||T|)

   function Create_With_Start
     (Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List) return Object;
   --  As following but in function form.

   procedure Create_With_Start
     (This   : in out Object;
      Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List);
   --  As previous, but adds a special Cr.Tasks.Starting_Pose task for each
   --  agent, which should be planned as the first task for each agent.
   --  These tasks are obtained via @Add_Starting_Tasks@

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Htn.Tasks.Task_Id;
      Fin   : in Htn.Tasks.Task_Id) return Costs;
   --  Returns infinite if Agent-task-task combination is not present

   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Htn.Tasks.Task_Id;
      Fin   : in     Htn.Tasks.Task_Id;
      Cost  : in     Costs);

private

   package ATT_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Costs, Ada.Strings.Hash, "=", Optimization."=");

   use Att_Maps;

   type Object is record
      Matrix : Map;
   end record;

   function Key (Agent : in String;
                 Ini   : in Htn.Tasks.Task_Id;
                 Fin   : in Htn.Tasks.Task_Id) return String;
   pragma Inline (Key);
   --  Construct a suitable key for indexing.

end Agpl.Cr.Cost_Matrix;
