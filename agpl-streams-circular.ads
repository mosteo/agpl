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

--  Circular stream. This is a buffering stream where the written data
--    can be read afterwards in typical producer/consumer fashion.

with Agpl.Streams.Observable;

with Ada.Finalization;

package Agpl.Streams.Circular is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   --  The size of the intermediate buffer is the maximum non-read data we
   --  can have:
   type Stream_type (Size : Stream_element_count) is new
      Streams.Observable.Stream_Type with private;
   type Stream_access is access all Stream_type;

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
   --  Says how many data has been written but not read:
   function Available_read (Stream : in Stream_type)
      return Stream_element_count;
   function Available_read (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_read);

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   --  Says how many data can be written to the stream:
   function Available_write (Stream : in Stream_type)
      return Stream_element_count;
   function Available_write (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_write);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Resets everything to the starting point
   procedure Reset (Stream : in out Stream_type);

private

   subtype Buffer_type is
      Stream_element_array;
   type Buffer_type_access is access all Buffer_type;

   --  Allocation is made in the first use to delay memory consumption:
   type Controlled_buffer_type (Size : Stream_element_count) is new
   Ada.Finalization.Controlled with
      record
         Data: Buffer_type_access;
      end record;

   procedure Finalize   (this: in out Controlled_buffer_type);

   type Stream_type (Size : Stream_element_count) is new
      Streams.Observable.Stream_Type
   with record
      Buffer    : Controlled_buffer_type (Size);
      Pos_read  : Stream_element_offset := 1; -- Next element to read.
      Pos_Write : Stream_element_offset := 1; -- Next element to write.
      Available_read : Stream_element_count  := 0; -- Pending for read data.
   end record;

end Agpl.Streams.Circular;
