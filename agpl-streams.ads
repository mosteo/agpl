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
--  $Id: agpl-streams.ads,v 1.3 2004/02/29 20:36:41 Jano Exp $

with Ada.Streams;
with Ada.Unchecked_Deallocation;

package Agpl.Streams is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------

   type Stream_access is access all Ada.Streams.Root_stream_type'Class;

   type Stream_element_array_access is access all
      Ada.Streams.Stream_element_array;

   ------------------------------------------------------------------------
   -- Utilities                                                          --
   ------------------------------------------------------------------------

   ----------
   -- Free --
   ----------
   procedure Free is new Ada.Unchecked_Deallocation (
      Ada.Streams.Stream_Element_Array, Stream_Element_Array_Access);
   procedure Free is new Ada.Unchecked_Deallocation (
      Ada.Streams.Root_Stream_Type'Class, Stream_Access);

   ---------------
   -- To_string --
   ---------------
   --  Returns a string having the characters in the stream array.
   --  Uses unchecked conversion
   function To_string (This : in Ada.Streams.Stream_element_array)
                       return    String;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------
   --  Uses unchecked conversion
   function To_Stream_Element_Array (This : in String)
                                     return    Ada.Streams.Stream_Element_Array;

   --  Facilities for children packages.

   subtype Root_Stream_Type      is Ada.Streams.Root_Stream_Type;

   subtype Stream_Element_Array  is Ada.Streams.Stream_Element_Array;
   subtype Stream_Element_Count  is Ada.Streams.Stream_Element_Count;
   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;
   use type Stream_Element_Array;
   use type Stream_Element_Count;
   use type Stream_Element_Offset;

end Agpl.Streams;
