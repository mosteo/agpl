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
--  The internal buffer will grow as needed to acommodate unread data.
--  The internal buffer isn't allocated until the first writting to save memory
--  usage.
--  Additionally, it is controlled.

with Agpl.Streams.Observable;

package Agpl.Streams.Circular_Unbounded is

   pragma Elaborate_Body;

   Memory_Limit_Reached : exception;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type
   is new Agpl.Streams.Observable.Stream_Type with private;

   type Stream_access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                            --
   ------------------------------------------------------------------------
   --  Max_Memory_Usage can cause an internal growing to fail.
   --  Grow_Factor stablishes how many unused space will be allocated upon
   --  buffer expansion. (100 causes as many used as free to be allocated)
   --  If Lazy, buffers allocation is not done until first writing.
   procedure Create (
      This              : in out Stream_Type;
      Max_Memory_Usage  : in     Stream_Element_Count := 1024 * 1024;
      Initial_Size      : in     Stream_Element_Count := 1024 * 4;
      Grow_Factor       : in     Natural              := 100;
      Lazy              : in     Boolean              := True);

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read(
      This : in out Stream_type;
      Item :    out Stream_Element_Array;
      Last :    out Stream_Element_Offset);

   procedure Write(
      This : in out Stream_type;
      Item : in     Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Peek                                                               --
   ------------------------------------------------------------------------
   --  Returns data but without effectively consuming it
   procedure Peek (
      This : in out Stream_type;
      Item :    out Stream_Element_Array;
      Last :    out Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Skip                                                               --
   ------------------------------------------------------------------------
   --  Skip these readable data
   procedure Skip (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_read (Stream : in Stream_type)
      return Stream_element_count;
   pragma Inline (Available_read);
   function Available_read (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_read);

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   --  Says how many data can be written to the stream:
   --  Only limited by the max_memory_usage parameter on creation.
   function Available_write (Stream : in Stream_type)
      return Stream_element_count;
   function Available_write (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_write);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Resets everything to the starting point (frees memory too)
   procedure Reset (Stream : in out Stream_type);

private

   procedure Finalize (This : in out Stream_Type);

   type Stream_type
   is new Agpl.Streams.Observable.Stream_Type with record
      Buffer    : Stream_Element_Array_Access;
      Pos_read  : Stream_Element_Offset       := 1; -- Next element to read.
      Pos_Write : Stream_Element_Offset       := 1; -- Next element to write.
      Available_read  : Stream_Element_Count  := 0; -- Data pending to be read.
      Available_write : Stream_Element_Count  := 0; -- Free space.

      Initial_Size    : Stream_Element_Count  := 0;
      Max_Memory_Usage: Stream_Element_Count  := 0;
      Grow_Factor     : Natural               := 0;
      Lazy            : Boolean               := True;
   end record;

end Agpl.Streams.Circular_Unbounded;
