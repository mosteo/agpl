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
--  functions.

with Agpl.Streams.Observable;

with Ada.Streams.Stream_IO;
use  Ada;

package Agpl.Streams.File is

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type is new Agpl.Streams.Observable.Stream_Type with private;
   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- The Index function shouldn't be called after this creation!!
   procedure Create (
      Stream :    out Stream_Type;
      From   : in     Ada.Streams.Stream_IO.File_Type);

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read(
      Stream : in out Stream_type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

	procedure Write(
      Stream : in out Stream_type;
      Item   : in     Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data has been written but not read:
   function Available_read (Stream : in Stream_type) 
      return Stream_element_count;
   function Available_read (Stream : in Stream_type) 
      return Natural;
   pragma Inline (Available_read);

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   -- Doesn't check free space, always return the largest possible value.
   function Available_write (Stream : in Stream_type) 
      return Stream_element_count;
   function Available_write (Stream : in Stream_type) 
      return Natural;
   pragma Inline (Available_write);

private

   type Stream_type is new Agpl.Streams.Observable.Stream_Type
   with record
      Back           : Agpl.Streams.Stream_Access; -- To read/write
      Available_Read : Stream_element_count;       -- Pending for read data.
   end record;

end Agpl.Streams.File;
