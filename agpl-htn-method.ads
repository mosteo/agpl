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

--  A method can take a non-primitive task and create a new list of tasks
--  that go towards solving the given task.

with Agpl.Htn.Plan_Node;
with Agpl.Htn.Tasks;

package Agpl.Htn.Method is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   function Apply (This : in Object; That : in Tasks.Object'Class)
                   return Plan_Node.Node_Access
   is abstract;
   --  Returning null means the method doesn't apply or failed
   --  The caller is responsible of doing something with the access
   --  or freeing it.

   procedure Finished_Child
     (This   : in     Object;
      Parent : in out Tasks.Object'Class;
      Child  : in     Tasks.Object'Class;
      Add    :    out Plan_Node.Node_Access;
      Done   :    out Boolean);
   --  Invoked when a child task has finished.
   --  @Add@ is a node that will hang under the Parent task.
   --  If the Parent has only a Task child, it will be replaced.
   --  If the Parent has an AND child, this new node will be merged in
   --  that list. (OR can't happen since they dissapear during plan expansion).
   --  Set Add to null if there's nothing else to do.
   --  @Done@ is used to signal early termination of the Parent,
   --  even if not all of its child tasks are finished.
   --  This default does nothing.

   procedure Finished_Task
     (This    : in     Object;
      T       : in out Tasks.Object'Class;
      Replace :    out Plan_Node.Node_Access);
   --  Invoked when a task finishes (primitive or not!). Primitive are notified
   --  first.
   --  if @Replace@ /= null, then this task is replaced with the new node and
   --  the plan is reexpanded (this in practice aborts the upwards notifications).
   --  This default does nothing.

   --  For more complex, dynamic re-expansions take a look at
   --  @Plan.Observer@.

end Agpl.Htn.Method;
