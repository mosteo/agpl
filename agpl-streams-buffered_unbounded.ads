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

--  Filter stream who allows to buffer for write or read some data. Just a 
--  convenience to use Circular_Unbounded.

with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;

package Agpl.Streams.Buffered_Unbounded is

   pragma Elaborate_Body;

   -- These parameters affect new instances (a-la Float_Io).
   Max_Buffer_Size : Stream_Element_Count := Stream_Element_Count'Last;
   Initial_Buffer  : Stream_Element_Count := 4096;
   Grow_Percent    : Positive             := 50;

   pragma Atomic (Max_Buffer_Size);
   pragma Atomic (Initial_Buffer);
   pragma Atomic (Grow_Percent);

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type (
      Back         : access Root_Stream_Type'Class)
   is new Agpl.Streams.Filter.Stream_Type with private;

   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   -- Will read from internal buffer
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   -- Will cache all data written
	procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   -- Cleans everything inside
   procedure Reset (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Flush_Read                                                         --
   ------------------------------------------------------------------------
   -- Tries to read so many data from the back stream.
   procedure Flush_Read (
      This  : in out Stream_type; 
      Count : in     Stream_Element_Count := Stream_Element_Count'Last);

   ------------------------------------------------------------------------
   -- Flush_Write                                                        --
   ------------------------------------------------------------------------
   -- Writes to the back buffer as many data as indicated or less if not as many is cached.
   procedure Flush_Write (
      This  : in out Stream_type; 
      Count : in     Stream_Element_Count := Stream_Element_Count'Last);

   ------------------------------------------------------------------------
   -- Get_Buffered_Read_Count                                            --
   ------------------------------------------------------------------------
   -- Returns the amount of read buffered data.
   function Get_Buffered_Read_Count (This : in Stream_Type) 
      return Stream_Element_Count;
   pragma Inline (Get_Buffered_Read_Count);

   ------------------------------------------------------------------------
   -- Get_Buffered_Write_Count                                           --
   ------------------------------------------------------------------------
   -- Returns the amount of written buffered data.
   function Get_Buffered_Write_Count (This : in Stream_Type) 
      return Stream_Element_Count;
   pragma Inline (Get_Buffered_Write_Count);

private

   package Circular renames Agpl.Streams.Circular_Unbounded;

   type Stream_type (
      Back         : access Root_Stream_Type'Class)
   is new Agpl.Streams.Filter.Stream_Type (
      Back => Back) with 
   record
      Buf_In   : Agpl.Streams.Circular_Unbounded.Stream_Type (
         Max_Memory_Usage => Max_Buffer_Size,
         Initial_Size     => Initial_Buffer,
         Grow_Factor      => Grow_Percent);
      Buf_Out  : Agpl.Streams.Circular_Unbounded.Stream_Type (
         Max_Memory_Usage => Max_Buffer_Size,
         Initial_Size     => Initial_Buffer,
         Grow_Factor      => Grow_Percent);
   end record;

end Agpl.Streams.Buffered_Unbounded;
