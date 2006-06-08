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

--  This package provides smart access pointers.
--  Thread *safe*.

with Agpl.Monitor;

with Ada.Finalization;  use Ada;

generic
   type Item (<>)   is limited private; -- Type.
   type Item_access is access Item;     -- This is the access we want safe.
   Item_id : String := "Anonymous";     -- For debug and error reporting.
package Agpl.Smart_access_Limited is

   pragma Elaborate_Body;

   Allocated_Access     : exception;
   --  Raised when trying to bind an already binded access.

   Deallocated_Access   : exception;
   --  Raised when trying to access an already deallocated access.

   Uninitialized_Access : exception;
   --  Raised when trying to access a uninitialized access.

   type Object is tagged private; -- Initially null...

   --  Is null?
   function Is_Null (This : in Object) return Boolean;

   Null_Access : constant Object;
   --  Get an uninitialized access.

   --  Initialization.
   --  May raise Allocated_Access.
   procedure Bind (This : in out Object; Data : in Item_Access);
   function  Bind (This : in     Item_Access) return Object;

   --  Get value
   --  Of course, if the access value is copied outside, error can occur.
   --  Since the Item is limited we can't return it
--     function Val (This : in Object) return Item;
--     function "+" (This : in Object) return Item renames Val;

   function Ref (This : in Object) return Item_Access;
   function "+" (This : in Object) return Item_Access renames Ref;

   --  Rebind. Like Unbind but assigning a new value.
   procedure Rebind
     (This  : in out Object;
      Data  : in     Item_Access;
      Force : in     Boolean := False);

   --  Unbinding:
   --  The value is no longer controlled
   --  Only valid if one reference (last) or forced.
   --  In other case, constraint error will be raised.
   --  Note that unbinding forcefully a pointer with more references will also
   --  nullify the other instances!
   procedure Unbind (This : in out Object; Force : in Boolean := False);

--     --  Serialization...
--     function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object;
--     for Object'Input use Input;
--
--     procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
--                       This : in Object);
--     for Object'Output use Output;

private

   --  Internal, debugging exceptions:
   Tracker_Already_Deallocated : exception;

   pragma Inline (Is_Null, Ref);
                  --  Val);

   --  Auxiliary type to do the counting.
   type Tracker_Type is record
      Data  : Item_access;          pragma Atomic (Data);
      Count : Natural     := 1;
      Alloc : Boolean     := False; -- To keep track of unalloc/dealloc status.
   end record;

   type Tracker_access is access Tracker_type;

   procedure Discount (This : in out Tracker_access);
   procedure Addcount (This : in out Tracker_access);
   pragma Inline (Discount, Addcount);

   --  The tracker is always allocated, even for null handlers, for simplicity
   --  and speed reasons.
   type Object is new Finalization.Controlled with record
      Tracker : Tracker_access := new Tracker_Type;
      pragma Atomic (Tracker);
   end record;

   procedure Adjust   (this : in out Object);
   procedure Finalize (this : in out Object);
   pragma Inline (Adjust, Finalize);

   Mutex : aliased Monitor.Semaphore;
   --  Global monitor for each operation.
   --  This means that there is only *one* mutex for every instantiation.
   --  This can cause slow down in multithreaded environment but...
   --  It is placed here because the finalization order will cause P_E if
   --  it is after the following Null_Access:

   Null_Access : constant Object :=
                   (Finalization.Controlled with
                    Tracker => new Tracker_Type'(Data  => null,
                                                 Count => 1,
                                                 Alloc => False));

end Agpl.Smart_access_Limited;
