------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
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
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: agpl-streams-memory_arrays.adb,v 1.1 2004/02/24 15:26:10 Jano Exp $

--  Allows reading from a string viewed as an stream:

package body Agpl.Streams.Memory_arrays is

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
      Pos    : Stream_element_offset renames Stream.Pos;
   begin
      if Pos > Stream.Data'Last then
         raise Constraint_Error;
      end if;
      if Item'Length > Stream.Data'Last - Pos + 1 then
         Item (Item'First .. Item'First + Stream.Data'Last - Pos) :=
            Stream.Data (Pos .. Stream.Data'Last);
         Last := Item'First + Stream.Data'Last - Pos;
         Pos  := Stream.Data'Last + 1;
      else
         Item := Stream.Data (Pos .. Pos + Item'Length - 1);
         Last := Item'Last;
         Pos  := Pos + Item'Length;
      end if;
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      Stream : in out Stream_type;
      Item   : in Stream_Element_Array) is
      Pos    : Stream_element_offset renames Stream.Pos;
   begin
      if Pos + Item'Length - 1 > Stream.Data'Length then
         raise Constraint_Error;
      end if;
      Stream.Data (Pos .. Pos + Item'Length - 1) := Item;
      Pos := Pos + Item'Length;
   end Write;

   ------------------------------------------------------------------------
   -- Index                                                              --
   ------------------------------------------------------------------------
   function Index (Stream : in Stream_type) return Stream_element_offset is
   begin
      return Stream.Pos - Stream.Data'First;
   end Index;

   ------------------------------------------------------------------------
   -- Set_index                                                          --
   ------------------------------------------------------------------------
   --  Set starting position for read/write (First is 1)
   procedure Set_index (
      Stream : in out Stream_type;
      Index  : in     Stream_element_offset) is
   begin
      Stream.Pos := Stream.Data'First + Index - 1;
   end Set_index;

   ------------------------------------------------------------------------
   -- End_of_stream                                                      --
   ------------------------------------------------------------------------
   function End_of_stream (Stream : in Stream_type) return Boolean is
   begin
      return Stream.Pos > Stream.Data.all'Last;
   end End_of_stream;

end Agpl.Streams.Memory_arrays;
