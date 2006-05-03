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
--  $Id: agpl-streams-circular.ads,v 1.1 2004/03/22 07:14:59 Jano Exp $

--  Wraps around a regular Stream_IO stream to provide Available_Read/Write
--  functions

--  with Text_Io;

package body Agpl.Streams.File is

   package Stream_IO renames Ada.Streams.Stream_IO;
   use type Stream_IO.Count;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The Index function shouldn't be called after this creation!!
   procedure Create
     (Stream : out Stream_type;
      From   : in Ada.Streams.Stream_IO.File_Type)
   is
   begin
      Stream.Back           :=
         Agpl.Streams.Stream_access (Stream_IO.Stream (From));
      Stream.Available_Read :=
         Stream_Element_Offset (Stream_IO.Size (From) -
                                Stream_IO.Index (From) +
                                1);
   end Create;

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read
     (Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      --Text_Io.Put_Line ("File remaining:" & Stream.Available_Read'Img);
      --Text_Io.Put_Line ("Attempting read of:" & Item'Length'Img);
      Ada.Streams.Read (Stream.Back.all, Item, Last);
      Stream.Available_Read := Stream.Available_Read -
                               (Last - Item'First + 1);
   end Read;

   procedure Write
     (Stream : in out Stream_type;
      Item   : in Stream_Element_Array)
   is
   begin
      Ada.Streams.Write (Stream.Back.all, Item);
   end Write;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data has been written but not read:
   function Available_read
     (Stream : in Stream_type)
      return   Stream_Element_Count
   is
   begin
      return Stream.Available_Read;
   end Available_read;

   function Available_read (Stream : in Stream_type) return Natural is
   begin
      return Natural (Stream_Element_Count'(Available_read (Stream)));
   end Available_read;

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   function Available_Write
     (Stream : in Stream_type)
      return   Stream_Element_Count
   is
      pragma Unreferenced (Stream);
   begin
      return Stream_Element_Count'Last;
   end Available_Write;

   function Available_Write (Stream : in Stream_type) return Natural is
      pragma Unreferenced (Stream);
   begin
      return Natural'Last;
   end Available_Write;

end Agpl.Streams.File;
