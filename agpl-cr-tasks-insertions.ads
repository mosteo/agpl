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

with Agpl.Cr;
with Agpl.Cr.Agent;
with Agpl.Cr.Agent.Handle;
with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Matrix;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Containers;
use  Agpl;

--  Insertion of tasks into agents

package Agpl.Cr.Tasks.Insertions is

   pragma Preelaborate;

   type Insertion_Procedures is access
     procedure (A          : in     Agent.Object'Class;
                T          : in     Htn.Tasks.Object'Class;
                New_Agent  :    out Agent.Handle.Object;
                Cost_Delta :    out Cr.Costs;
                Cost_Total :    out Cr.Costs;
                Success    :    out Boolean);

   procedure Before_Id (List    : in out Htn.Tasks.Containers.Lists.List;
                        Job     : in     Htn.Tasks.Object'Class;
                        Id      : in     Htn.Tasks.Task_Id;
                        Is_Last : in     Boolean := False);
   --  Insert in the given list, before task Id
   --  or else, if Is_Last, append at end.
   --  If Id is not found then raise Program_Error

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Htn.Tasks.Object'Class;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Cr.Costs;
                     Cost_Total :    out Cr.Costs;
                     Success    :    out Boolean);
   --  Tests the best place where to insert task T for the agent.
   --  The task list is not reordered, only each place is tried.
   --  Returns a copy of @A@ with the task inserted in New_Agent.
   --  Returns the increase in cost in @Cost_Delta@
   --  Returns total cost for agent agenda in @Cost_Total@
   --  New_Agent will be of same class than A
   --  Success will be false if the agent can't insert T at any place.

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Htn.Tasks.Object'Class;
                     C          : in     Cost_Matrix.Object;
                     Not_Before : in     Natural;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Cr.Costs;
                     Cost_Total :    out Cr.Costs;
                     Success    :    out Boolean);
   --  Tests the best place where to insert task T for the agent.
   --  The task list is not reordered, only each place is tried.
   --  Costs aren't given by the agent but taken from the cost_matrix
   --  Returns a copy of @A@ with the task inserted in New_Agent.
   --  Returns the increase in cost in @Cost_Delta@
   --  Returns total cost for agent agenda in @Cost_Total@
   --  New_Agent will be of same class than A
   --  Success will be false if the agent can't insert T at any place.

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Htn.Tasks.Object'Class;
                     Not_Before : in     Natural;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Cr.Costs;
                     Cost_Total :    out Cr.Costs;
                     Success    :    out Boolean);
   --  As previous, but with Not_Before you can force an amount of tasks to
   --  not be considered

   procedure Greedy (Ass       : in     Assignment.Object;
                     T         : in     Htn.Tasks.Object'Class;
                     Costs     : in     Cost_Matrix.Object;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Success   :    out Boolean);
   --  Insert a task in the best place of the best agent of an assignment
   --  The results are given in New_Ass, with Success true.

   procedure Greedy (Ass       : in     Assignment.Object;
                     Tasks     : in     Htn.Tasks.Containers.Lists.List;
                     Costs     : in     Cost_Matrix.Object;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Inserted  :    out Htn.Tasks.Task_Id);
   --  Insert the best task of the list in the best agent.
   --  Just *one* task is inserted.
   --  Inserted can be No_Task if failure.

end Agpl.Cr.Tasks.Insertions;
