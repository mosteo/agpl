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

with Agpl.Smart_Access_Limited;

with Ada.Streams;

pragma Elaborate_All (Agpl.Smart_Access_Limited);

generic
   type Item (<>)   is private;         -- Type.
   type Item_access is access Item;     -- This is the access we want safe.
   Item_id : String := "Anonymous";     -- For debug and error reporting.
package Agpl.Smart_access is

   pragma Elaborate_Body;

   package Lim is new Smart_Access_Limited (Item, Item_Access, Item_Id);

   type Object is new Lim.Object with private;

   Null_Access : constant Object;
   --  Get an uninitialized access.

   --  Get value
   --  Of course, if the access value is copied outside, error can occur.
   function Val (This : in Object) return Item;
   function "+" (This : in Object) return Item renames Val;

   --  Serialization...
   function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object;
   for Object'Input use Input;

   procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
                     This : in Object);
   for Object'Output use Output;

private

   pragma Inline (Val);

   type Object is new Lim.Object with null record;

   Null_Access : constant Object := (Lim.Null_Access with null record);

end Agpl.Smart_access;
