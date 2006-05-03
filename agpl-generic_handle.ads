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

--  A container for indefinite objects allowing an easier storage for them.
--  'Read and 'Write are implemented, so this type can be safely serialized.

with Ada.Finalization;
with Ada.Streams;

generic
   type Item (<>) is private;
package Agpl.Generic_Handle is

   pragma Preelaborate;

   type Item_Access is access Item;

   No_Data : exception;

   type Object is tagged private;

   function Set (This : in Item) return Object;
   function "+" (This : in Item) return Object renames Set;
   --  Creation

   procedure Set (This : in out Object; X : in Item);
   --  Creation

   function Get (This : in Object) return Item;
   function "+" (This : in Object) return Item renames Get;
   --  Extraction. May raise No_Data if uninitialized.

   function Ref (This : in Object) return Item_Access;
   --  Reference to the held item.

   procedure Clear (This : in out Object);
   --  Make it uninitialized.

   function Is_Valid (This : in Object) return Boolean;
   --  Says if it has been initialized.

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object);
   for Object'Read use Read;

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object);
   for Object'Write use Write;

private

   type Object is new Ada.Finalization.Controlled with record
      Data : Item_Access;
   end record;

   procedure Adjust   (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Generic_Handle;
