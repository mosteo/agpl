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

--  Tasks of the network.

--  Tasks can be either primitive or compound. A primitive task is directly
--  doable by some known mean (also called operator). A compound task must be
--  decomposed in other tasks (either compound and/or primitive) until all
--  tasks are primitive. At this point we say we have a valid Plan.

with Ada.Finalization;
with Ada.Streams;

package Agpl.Htn.Tasks is

   pragma Elaborate_Body;

   Id_Error : exception;

   type Object is abstract tagged private;
   type Object_Access is access Object'Class;

   type Task_Id is new Natural;
   No_Task : constant Task_Id;

   procedure Assign_Id (This : in out Object);
   --  This should be used when creating a task to assign a unique ID to it.

   procedure Set_Next_Id (X : in Positive);
   --  For debug

   function Get_Id (This : in Object) return Task_Id;
   --  Returns the id for this task.
   --  Raises Id_Error if the task has not been properly initialized.

   function Is_Primitive (This : in Object) return Boolean is abstract;

   function To_String (This : in Object) return String;
   --  Human readable task description.
   --  This default returns "Task #id".

   procedure Delete (This : in out Object_Access);
   --  Used to free objects of Object'Class type

   function Serialize (This : in Object) return String;
   --  String representation of all relevant task info.
   --  Default implementation calls Do_Serialize.
   --  Override if needed (pointers or so).

   function Do_Serialize (This : in Object'Class) return String;
   --  Shortcut for tasks without pointers and so.
   --  It will just use the 'Output attribute in a memory stream.

   function Do_Unserialize (This : in String) return Object'Class;
   --  Reverse operation of Do_Serialize.

   function Get_New_Id return Task_Id;
   --  Get a new unique id (thread safe).

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Object_Access);
   --  Will dispatch to .all'Write

   --  for Object_Access'Write use Write;

   function Same_Id (L, R : in Object'Class) return Boolean;
   --  Equality by id.

private

   No_Task : constant Task_Id := 0;

   type Object is abstract new Ada.Finalization.Controlled with record
      Id : Task_Id := No_Task;
   end record;

   procedure Initialize (This : in out Object);

end Agpl.Htn.Tasks;
