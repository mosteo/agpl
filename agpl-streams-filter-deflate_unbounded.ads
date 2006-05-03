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
--  $Id: agpl-streams-deflate.ads,v 1.1 2004/03/29 19:13:33 Jano Exp $

--  Filtering stream which compresses on writting and decompresses on reading.

with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;

with Zlib.Streams;

package Agpl.Streams.Filter.Deflate_Unbounded is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------

   -- Raised when there is remaining data to be written in back buffer and 
   -- a premature close is attempted.
   Pending_Data : exception;
   
   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type is new Filter.Stream_Type with private;

   type Stream_access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Max_Memory_Usage can cause an internal growing to fail.
   -- Grow_Factor stablishes how many unused space will be allocated upon
   -- buffer expansion. (100 causes as many used as free to be allocated)
   -- If Lazy, buffers allocation is not done until first writing.
   procedure Create (
      This              : in out Stream_Type;
      Back              : access Ada.Streams.Root_Stream_Type'Class;
      Max_Memory_Usage  : in     Stream_Element_Count := 1024 * 1024;
      Initial_Size      : in     Stream_Element_Count := 1024 * 4;
      Grow_Factor       : in     Natural              := 100;
      Lazy              : in     Boolean              := true);

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   -- May raise some exception if not Ready_For_Writing
	procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- Will fail if there is data pending to be flushed/written
   procedure Close (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   -- Cleans everything inside, including stats.
   procedure Reset (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Close_With_Abort                                                   --
   ------------------------------------------------------------------------
   -- Will always succeed, data pending will not be written
   procedure Close_With_Abort (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   -- Tries to read at least this many data from the compressed stream to
   -- allow making some uncompressed data available
   procedure Prefetch (
      This  : in out Stream_Type; 
      Count : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Fetch                                                              --
   ------------------------------------------------------------------------
   -- Read from back until @Count@ uncompressed data is available.
   procedure Fetch (
      This : in out Stream_Type; Count : in Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Flush                                                              --
   ------------------------------------------------------------------------
   -- Flushes the Z streams (reduces compression ratio).
   procedure Flush (This : in out Stream_type);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data is ready to be read
   function Available_Read (This : in Stream_Type) return Stream_Element_Count;
   pragma Inline (Available_Read);

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   -- Returns Max_Memory_Usage
   function Available_Write (This : in Stream_Type) 
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Everything_Written                                                 --
   ------------------------------------------------------------------------
   -- True if no data is pending to be flushed.
   -- Use <code>Flush</code>.
   function Everything_Written (This : in Stream_Type) return Boolean;

   ------------------------------------------------------------------------
   -- Get_Write_Ratio                                                    --
   ------------------------------------------------------------------------
   -- Gives the write compression ratio (Gained size / Original Size)
   function Get_Write_Ratio (This : in Stream_Type) return Float;

   ------------------------------------------------------------------------
   -- Get_Read_Ratio                                                    --
   ------------------------------------------------------------------------
   -- Gives the read compression ratio (Gained / Original)
   function Get_Read_Ratio (This : in Stream_Type) return Float;

private

   package Circular renames Agpl.Streams.Circular_Unbounded;

   type Stream_type is new Filter.Stream_Type with 
   record
      Zin,
      Zout    : Zlib.Streams.Stream_type;

      Buf_In  : aliased Circular.Stream_Type;
      Buf_Out : aliased Circular.Stream_Type;

      Unflushed_ZOut  : Natural := 0; -- Amount in ZBuffer not flushed

      -- Data for ratios
      -- These integers can be < 0 due to the algorithm nature
      Gained_Written       : Integer := 0; 
      Uncompressed_Written : Natural := 0;
      Gained_Read          : Integer := 0;
      Uncompressed_Read    : Natural := 0;
   end record;

   ------------------------------------------------------------------------
   -- Single_Read                                                        --
   ------------------------------------------------------------------------
   -- Read as requested from the compressed stream. No expectations on un-
   -- compressed data will be tried to meet.
   procedure Single_Read (
      This   : in out Stream_type;
      Count  : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Attempt_Read                                                       --
   ------------------------------------------------------------------------
   -- Works on internal circular buffers
   -- Read as many data as possible from the back stream to the circular
   -- buffer, until @Count@ data is available uncompressed.
   procedure Attempt_Read (
      This   : in out Stream_type;
      Count  : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Attempt_write                                                      --
   ------------------------------------------------------------------------
   -- Works on internal circular buffers
   -- Write as many data as possible as result of compression
   procedure Attempt_Write (This : in out Stream_type);

   procedure Initialize (This : in out Stream_Type);
   procedure Finalize   (This : in out Stream_Type);

end Agpl.Streams.Filter.Deflate_Unbounded;
