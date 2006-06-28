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
--  $Id: agpl-streams-memory_arrays.ads,v 1.1 2004/02/24 15:26:10 Jano Exp $

--  Allows reading from an array viewed as an stream:
--  or writing to it.
--  Mixing the two operations is erroneous.

with Ada.Streams;                use Ada.Streams;

package Agpl.Streams.Memory_arrays is

   pragma Preelaborate;

   --  New stream type:
   --  Takes an access to an element array. It should remain allocated
   --  during the life of this stream.
   --  It will not be deallocated
   type Stream_type (
      Data  : access Stream_element_array) is new
      Ada.Streams.Root_Stream_Type with private;

   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (Stream : in out Stream_Type;
                   Item   :    out Stream_Element_Array;
                   Last   :    out Stream_Element_Offset);
   pragma Inline (Read);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   --  Can cause constraint_error if the supplied buffer is exhausted.
   procedure Write (Stream : in out Stream_Type;
                    Item   : in     Stream_Element_Array);
   pragma Inline (Write);

   ------------------------------------------------------------------------
   -- Index                                                              --
   ------------------------------------------------------------------------
   --  Amount of data written / read.
   function Index (Stream : in Stream_type) return Stream_element_offset;

   ------------------------------------------------------------------------
   -- Set_index                                                          --
   ------------------------------------------------------------------------
   --  Set starting position for read/write (First is 1)
   procedure Set_index (
      Stream : in out Stream_type;
      Index  : in     Stream_element_offset);

   ------------------------------------------------------------------------
   -- End_of_stream                                                      --
   ------------------------------------------------------------------------
   function End_of_stream (Stream : in Stream_type) return Boolean;

private

   type Stream_type (
      Data  : access Stream_element_array) is new Ada.Streams.Root_Stream_Type
   with record
      Pos : Stream_element_offset := Data.all'First;
   end record;

end Agpl.Streams.Memory_arrays;
