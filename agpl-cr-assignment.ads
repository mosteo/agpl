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

--  An assignment says who must perform each task in a plan.

with Agpl.Cr.Agent.Lists;
with Agpl.Cr.Agent.Maps;
with Agpl.Cr.Cost_Matrix;
with Agpl.Htn.Plan;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Lists;

package Agpl.Cr.Assignment is

   pragma Elaborate_Body;

   type Object is tagged private;

   --   type Object is tagged private;

   procedure Add
     (This     : in out Object;
      Agent    : in     Cr.Agent.Object'Class;
      The_Task : in     Htn.Tasks.Object'Class);
   --  Add a task to some agent

   procedure Clear (This : in out Object);

   procedure Fill_Owners (This : in Object; Plan : in out Htn.Plan.Object);
   --  Fill the owners of tasks in a plan with this assignment.

   function Get_Agent (This : in Object; Name : in String)
                       return Agent.Object'Class;
   --  Gets an agent copy with all its tasks.

   function Get_Most_Costly_Agent (This : in Object) return Agent.Object'Class;
   --  Get a copy of the agent with longest task list

   procedure Set_Agent (This : in out Object; Agent : in Cr.Agent.Object'Class);
   --  Add or replace an agent and its tasks

   function Get_Agents (This : in Object)
                        return Agent.Lists.List;
   --  Get all agents with its tasks.

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Lists.List;

   function Get_All_Tasks (This : in Object) return Htn.Tasks.Lists.List;
   --  Return all tasks in the assignment, regardless of owner agent.

   function Get_Tasks (This : in Object; Agent : in Cr.Agent.Object'Class)
                       return Htn.Tasks.Lists.List;
   --  Says the tasks assigned to a particular agent.

   function Get_Max_Min_Cost (This : in Object) return Costs;
   function Get_Max_Min_Cost (This : in Object;
                              C    : in Cost_Matrix.Object) return Costs;
   --  Says the worst of all the agent total costs.

   function Get_Cummulative_Cost (This : in Object) return Costs;
   function Get_Cummulative_Cost (This : in Object;
                                  C    : in Cost_Matrix.Object) return Costs;
   --  Says the sum of all agent costs.

   function Get_Cost (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs;
   function Get_Cost (This      : in Object;
                      C         : in Cost_Matrix.Object;
                      Criterion : in Assignment_Criteria) return Costs;
   --  Uses one of the two previous according to the Criterion

   function Invalid_Assignment return Object;
   --  Returns an invalid assignment.

   function Is_Valid (This : in Object) return Boolean;

   procedure Set_Valid (This : in out Object; Valid : in Boolean := True);

   function Freeze_Plan (This : in Object;
                         P    : in Htn.Plan.Object)
                         return    Htn.Plan.Object;
   --  This will take a plan that contains a superset of the tasks in the
   --  assignment. If the plan contains OR nodes, these will be replaced with
   --  the branches used by the assignment.
   --  If some incompability is detected (tasks in This but not in P, or
   --  sibling tasks used in This), an exception will be raised.
   --  Note that if P > This, the plan can be just partially frozen.

   --  DEBUG

   procedure Print_Assignment (This : in Object);

private

      type Object is tagged record
      Agents : Agent.Maps.Map;

      Ok     : Boolean := True;
      --  An assignment can be invalid.
   end record;

end Agpl.Cr.Assignment;
