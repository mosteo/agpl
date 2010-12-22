 

with Agpl.Cr.Agent;
with Agpl.Cr.Agent.Lists;
with Agpl.Cr.Assigner;
with Agpl.Cr.Assignment;
with Agpl.Cr.Controller_Observer;
with Agpl.Cr.Controller_Observer.Lists;
with Agpl.Cr.Demiurge;
with Agpl.Htn.Plan;
with Agpl.Htn.Plan.Lists;
with Agpl.Htn.Tasks;

with Ada.Calendar;

package Agpl.Cr.Controller is

   pragma Elaborate_Body;

   --  This protected type aggregates agents and tasks, and re-plans as needed.
   --  Its @Run@ method must be periodically invoked for things to go properly.
   protected type Object is

      procedure Add (This : in Assigner.Object'Class);
      --  Add a new algorithm for tasks assignation.

      procedure Add (This : in Agent.Object'Class);
      --  Add a new agent to the mix.

      procedure Add (This : in Htn.Plan.Object);
      --  A new initial set of things to be performed.
      --  Adding a new plan will reset the internal tasks.
      --  A new planification/assignation will be computed.

      procedure Add (This : in Htn.Tasks.Object'Class);
      --  A new task to be added to the current @Plan@.
      --  Will cause a replanification/assignation.

      procedure Add (This : in Controller_Observer.Object'Class);
      --  A new observer for changed plans.

      function Check_Completion return Boolean;
      --  Check if all the agents have finished.

      function Get_Plan return Htn.Plan.Object;
      --  Gets a duplicate of the current plan in use.

      procedure Replace_Plan (This : in Htn.Plan.Object);
      --  Replace current plan with This and expand it.

      procedure Replace_Plan (Plans : in Htn.Plan.Lists.List);
      --  Expand and assign all these plans and keep the best one.

      procedure Replan;
      --  Forces a new replanification.

      procedure Run;
      --  Agents are called for control actions.

      --  DEBUG PROCEDURES --
      procedure Print_Report;
      --  Dumps a report of status to Stdout

   private

      procedure Mark_Done
        (A : in Agent.Object'Class; T : in Htn.Tasks.Object'Class);
      --  Signal this controller that a task has been finished by its agent.
      --  The task will be removed from the agent and plan.

      procedure Pass_Tasks_To_Agents;
      --  Copies primitive tasks in current found assignment to the agents.

      Agents        : Agent.Lists.List;
      --  The tasks for each agent are here. They will be removed during progress.

      Assignments   : Assignment.Object;
      --  The original assignment. No tasks are removed from here.

      Demi          : Cr.Demiurge.Object; -- Assignment framework.
      Plan          : Htn.Plan.Object;    -- Current plan.

      Plan_Cost     : Cr.Costs;           -- The estimation for current plan.
      Plan_Start    : Ada.Calendar.Time;

      Assign_Ok     : Boolean := False;   -- Says if we have a valid assignment.

      Observers     : Controller_Observer.Lists.List;
   end Object;

   type Object_Access is access all Object;

end Agpl.Cr.Controller;
