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
--  $Id: adagio-g2-packet-parsing.ads,v 1.5 2004/01/21 21:05:26 Jano Exp $

--  Functions to obtain G2 packets.
with Agpl.Streams;
with Agpl.Trace;

with Ada.Streams;

package Agpl.G2.Packet.Parsing is

   --  Entirely from stream:
   function From_stream (
      Stream       : access Ada.Streams.Root_stream_type'Class)
      return Packet.Object;

   --  Returns a newly allocated G2 packet from a stream element array.
   function From_element_array (Data : in Streams.Stream_element_array)
                                return    Packet.Object;

   function From_stream_element_array (Data : in Streams.Stream_element_array)
      return Packet.Object renames From_Element_Array;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  Facility for checking a stream:
   type Object is limited private;

   type Available_function is access
      function (Stream : access Ada.Streams.Root_stream_type'Class)
         return Natural;

   --  Initialize:
   procedure Create (
      This         : out Object;
      Link         : in  Agpl.Streams.Stream_access;
      Available    : in  Available_function);

   --  Call this function each time a packet is to be checked
   --  Will return Null_packet until a packet is fully acquired:
   --  If aggresive, will try full packet if available.
   --  If not, will return after reading control, after reading length
   procedure Check (
      this         : in out Object;
      Aggresive    : in     Boolean := True;
      Result       :    out Packet.Object);

   -----------
   -- Debug --
   -----------
   --  Draws a tree of the packet:
   procedure Trace_tree (
      this   : in Packet.Object;
      Level  : in Trace.Warning_Levels := Trace.Debug;
      Indent : in Natural := 0);

private

   --  Returns a newly allocated G2 packet from a stream.
   --  Pre: The stream holds enough ready data for the packet or
   --    it will try to block.
   --  Pre: The control byte and len have been read from the stream and
   --    the stream position is in the name field.
   function From_stream(
      Control_byte : in Control_byte_type;
      Length       : in Natural;
      Stream       : access Ada.Streams.Root_stream_type'Class)
      return Packet.Object;
   pragma Inline (From_stream);

   use type Ada.Streams.Stream_element_offset;

   --  Returns a newly allocated G2 packet from a stream.
   --  Pre: The stream holds enough ready data for the packet or
   --    it will try to block.
   --  Pre: The control byte and len have been read from the stream and
   --    the stream position is in the name field.
   procedure From_stream(
      Control_byte : in Control_byte_type;
      Length       : in Natural;
      Stream       : access Ada.Streams.Root_stream_type'Class;
      Child        : out Packet.Child_access;
      Read         : out Natural);

   --  Returns a G2 packet.
   --  It is read fully from the beggining of the stream, assuming there
   --  enough data in it.
   --  Control byte is also taken from the stream; Length as well.
   procedure From_stream (
      Stream       : access Ada.Streams.Root_stream_type'Class;
      Child        : out Packet.Child_access;
      Read         : out Natural);

   --  Draws a tree of the packet:
   procedure Trace_tree (
      this   : in Child_access;
      Level  : in Trace.Warning_Levels := Trace.Debug;
      Indent : in Natural := 0);

   type Pipe_status_type is (Ready, Control_done, Length_done, Skipping);
   type Stream_access is access all Ada.Streams.Root_stream_type'Class;

   type Object is limited record
      Link         : Agpl.Streams.Stream_access;
      Pipe_status  : Pipe_status_type := Ready;
      Control_byte : Control_byte_type;
      Packet_len   : Natural;
      Buffer       : Agpl.Streams.Stream_element_array_access;
      Available    : Available_function;

      Debug_prev_packet : Ustring := U ("");
      Debug_curr_packet : Ustring := U ("");
   end record;

end Agpl.G2.Packet.Parsing;
