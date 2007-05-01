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

--  A basic interactive agent.

with Agpl.Htn.Plan;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Containers;
with Agpl.Htn.Tasks.Primitive;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

--  with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Tags;

package Agpl.Cr.Agent is

   pragma Preelaborate;

   type Object is abstract tagged private;

   procedure Add_Task
     (This : in out Object; The_Task : in Htn.Tasks.Object'Class);
   --  Assign this task to the end of this agent TO DO list.

   procedure Clear_Tasks (This : in out Object);
   --  Remove all assigned tasks.

   procedure Execute
     (This     : in out Object;
      The_Task : in out Htn.Tasks.Primitive.Object'Class;
      Plan     : in out Htn.Plan.Object;
      Done     :    out Boolean) is abstract;
   --  Called periodically to allow the agent to perform the required
   --  control actions related with a primitive task.
   --  Changing the plan can cause replanning.
   --  Done must be set to true when the task is finished.

   procedure Execute_When_Idle
     (This : in out Object;
      Plan : in out Htn.Plan.Object);
   --  Will be called when no tasks pending.
   --  Default does nothing.

   function Get_Cached_Cost (This : in Object) return Costs;
   --  Returns the cost that was estimated when starting execution.

--     function Get_Elapsed (This : in Object) return Duration;
--     --  Returns the elapsed time since starting execution (Mark_Start).

   function Get_Cost (This : in Object; The_Task : in Htn.Tasks.Object'Class)
                      return Costs;
   --  Must say how takes for the agent to do The_Task from its current pose.
   --  This default will call Get_Cost (From, To) with From being instance of
   --  Agpl.Cr.Tasks.Starting_Pose for the agent.
   --  Unless The_Task is already of Starting_Pose'Class, when this returns 0.0.
   --  In practice, this means you have to implement Get_Cost (From, To) taking
   --  into account that From can be a Starting_Pose task.

   function Get_Cost (This : in Object; From, To : in Htn.Tasks.Object'Class)
                      return Costs;
   --  Must say how takes for the robot to do To assuming his last assignment
   --  was From.
   --  This default return Costs'Last.
   --  Should return Costs'Last for undoable tasks, never raise Constraint_Error.

   function Get_Plan_Cost (This : in Object) return Costs;
   --  Says the cost of doing everything in the TO DO list.
   --  Should return Costs'Last for undoable plans, never raise Constraint_Error.

   function Get_Plan_Cost (This  : in Object;
                           Tasks : in Htn.Tasks.Containers.Lists.List) return Costs;
   --  Says the cost of this plan, not the one in the robot.
   --  Should return Costs'Last for undoable plans, never raise Constraint_Error.

   function Get_Name (This : in Object) return String;

   function Get_First_Task (This : in Object) return Htn.Tasks.Object'Class;

   function Get_Last_Task  (This : in Object) return Htn.Tasks.Object'Class;

   function Get_Task_Count (This : in Object) return Natural;

   function Get_Tasks (This : in Object)
                       return Htn.Tasks.Containers.Lists.List;

   procedure Modify_Task_List (This     : in out Object;
                               Modifier : access procedure
                                 (Tasks : in out Htn.Tasks.Containers.Lists.List));
   --  Will call Modifier with a in-place modifiable task list, to avoid
   --  redundant copying.

   procedure Set_Task (This : in out Object;
                       T    :        Htn.Tasks.Object'Class);
   --  Removes any other task currently assigned.

   procedure Set_Tasks (This  : in out Object;
                        Tasks : in     Htn.Tasks.Containers.Lists.List);
   --  Set all the ordered tasks that must conform the TO DO list;

   function Has_Tasks (This : in Object) return Boolean;
   --  Says if the TO DO list is not empty.

--     procedure Mark_Start (This : in out Object);
   --  Marks the execution is starting now, and caches the estimated cost.

   procedure Remove_First_Task (This : in out Object);

   procedure Remove_Task (This : in out Object; Id : in Htn.Tasks.Task_Id);
   --  Remove this task from this agent TO DO list.

   procedure Set_Name (This : in out Object; Name : in String);

   procedure Print_Plan_Cost (This : in Object);
   --  For debugging.

   --------------------
   --  TASK HANDLERS --
   --------------------
   type Task_Executer is access procedure (This : in out Object'Class;
                                           Job  : in out Htn.Tasks.Object'Class;
                                           Done : in out Boolean);

   procedure Add_Task_Executer (This : in out Object;
                                Job  : in     Ada.Tags.Tag;
                                Exe  : in     Task_Executer);
   --  Add a new executer for a task kind.

   procedure Call_Executer (This : in out Object;
                            Job  : in out Htn.Tasks.Object'Class;
                            Done : in out Boolean);
   --  No need to override this. It simply dispatchs to a registered executer.

   function Has_Executer (This : in Object;
                          Job  : in Htn.Tasks.Object'Class) return Boolean;
   --  Says if there's a registered executer for this task.

private

   package Executer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Task_Executer, Ada.Strings.Hash, "=", "=");

   type Object is abstract tagged record
      Name  : Ustring;
      --  Mnemonic for the agent.

      Cost  : Costs;
--      Start : Ada.Calendar.Time;

      Tasks : Htn.Tasks.Containers.Lists.List;
      --  The TO DO list for this agent.

      Execs : Executer_Maps.Map;
      --  Registered task executers for this agent.
   end record;

end Agpl.Cr.Agent;
