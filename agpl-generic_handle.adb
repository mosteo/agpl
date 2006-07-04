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

with Ada.Unchecked_Deallocation;

package body Agpl.Generic_Handle is


   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Object) is
   begin
      This.Data := new Item'(This.Data.all);
   end Adjust;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      Finalize (This);
   end Clear;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation (Item, Item_Access);
   begin
      Free (This.Data);
   end Finalize;

   ---------
   -- Set --
   ---------

   function Set (This : in Item) return Object is
   begin
      return (Ada.Finalization.Controlled with Data => new Item'(This));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Object; X : in Item) is
   begin
      This := Set (X);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (This : in Object) return Item is
   begin
      if This.Data = null then
         raise No_Data;
      else
         return This.Data.all;
      end if;
   end Get;

   -----------------
   -- Null_Object --
   -----------------

   function Null_Object return Object is
   begin
      return (Ada.Finalization.Controlled with null);
   end Null_Object;

   ---------
   -- Ref --
   ---------

   function Ref (This : in Object) return Item_Access is
   begin
      if This.Data = null then
         raise No_Data;
      else
         return This.Data;
      end if;
   end Ref;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Object) return Boolean is
   begin
      return This.Data /= null;
   end Is_Valid;

   ----------
   -- Read --
   ----------
   --  We use a boolean to signal a valid data in the stream.
   --  This is a waste since a byte would suffice, but I don't care.
   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object)
   is
      Valid : Boolean;
   begin
      Finalize (This);

      Boolean'Read (Stream, Valid);
      if Valid then
         This.Data := new Item'(Item'Input (Stream));
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object)
   is
   begin
      if This.Data = null then
         Boolean'Write (Stream, False);
      else
         Boolean'Write (Stream, True);
         Item'Output (Stream, This.Data.all);
      end if;
   end Write;

end Agpl.Generic_Handle;
