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

package body Agpl.Smart_Access is

   ---------
   -- Val --
   ---------
   --  Get value
   --  Of course, if the access value is copied outside, error can occur.
   function Val (This : in Object) return Item is
   begin
      return This.Ref.all;
   end Val;

   -----------
   -- Input --
   -----------

   function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object
   is
      Valid : constant Boolean := Boolean'Input (S);
      This  : Object;
   begin
      if Valid then
         declare
            Data : constant Item_Access := new Item'(Item'Input (S));
         begin
            This.Bind (Data);
         end;
      end if;
      return This;
   end Input;

   ------------
   -- Output --
   ------------

   procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
                     This : in Object)
   is
   begin
      Boolean'Output (S, Is_Null (This));
      if not Is_Null (This) then
         Item'Output (S, Val (This));
      end if;
   end Output;

end Agpl.Smart_Access;
