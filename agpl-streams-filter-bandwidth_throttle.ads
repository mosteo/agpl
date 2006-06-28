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

--  Filter stream which uses an Agpl.Bandwidth_Throttle to limit data rates.
--  Provides statistics about global data rate and "instantaneous" data rate.
--  The instantaneous data rate averaging can be configured.

with Agpl.Average_Queue;
with Agpl.Average_Queue.Timed; pragma Elaborate_All (Agpl.Average_Queue.Timed);
with Agpl.Bandwidth_Throttle;
with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;
with Agpl.Types;

with Ada.Calendar;
use  Ada;

package Agpl.Streams.Filter.Bandwidth_Throttle is

   --  pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   --  Some internal error happened, this stream should be reset:
   --  Should never happen.
   Synchronization_Lost : exception;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type is new Agpl.Streams.Filter.Stream_Type with private;

   type Stream_access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The bandwidth throttles can be null to imply unlimited bandwidth
   procedure Create (
      This              : in out Stream_Type;
      Back              : access Ada.Streams.Root_Stream_Type'Class;
      Throttle_In       : in     Agpl.Bandwidth_Throttle.Object_Access;
      Throttle_Out      : in     Agpl.Bandwidth_Throttle.Object_Access;
      Max_Buffer_Size   : in     Stream_Element_Count :=
                                    Stream_Element_Count'Last;
      Initial_Size      : in     Stream_Element_Count := 4096;
      Grow_Percent      : in     Natural              := 50;
      Averaging_Slots   : in     Natural              := 5;
      Slot_Duration     : in     Natural              := 1000);

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
   --  Non-blocking. Will cache data which cannot be immediately written.
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Cleans everything inside, including stats.
   procedure Reset (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Flush                                                              --
   ------------------------------------------------------------------------
   --  Tries to write again data which was cached.
   procedure Flush (This : in out Stream_type);

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Raises Unknown
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Returns Max_Memory_Usage
   function Available_Write (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Get_Buffered_Write_Count                                           --
   ------------------------------------------------------------------------
   --  Returns the amount of written buffered data pending to be sent.
   function Get_Buffered_Write_Count (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Get_Total_Read                                                     --
   ------------------------------------------------------------------------
   --  How many bytes have been read from this throttle.
   function Get_Total_Read (This : in Stream_Type) return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Get_Current_Write_Rate                                             --
   ------------------------------------------------------------------------
   --  Gives the instantaneous writting speed.
   function Get_Current_Write_Rate (This : access Stream_Type) return Types.Data_Rate;

   ------------------------------------------------------------------------
   -- Get_Current_Read_Rate                                              --
   ------------------------------------------------------------------------
   --  Gives the reading speed since creation or last reset.
   function Get_Current_Read_Rate (This : access Stream_Type) return Types.Data_Rate;

   ------------------------------------------------------------------------
   -- Get_Global_Write_Rate                                              --
   ------------------------------------------------------------------------
   --  Gives the writting speed since creation or last reset.
   function Get_Global_Write_Rate (This : in Stream_Type) return Types.Data_Rate;

   ------------------------------------------------------------------------
   -- Get_Global_Read_Rate                                               --
   ------------------------------------------------------------------------
   --  Gives the reading speed since creation or last reset.
   function Get_Global_Read_Rate (This : in Stream_Type) return Types.Data_Rate;

private

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Stream_Type);

   package Circular renames Agpl.Streams.Circular_Unbounded;
   use type Types.Data_Rate;
   use type Calendar.Time;

   package Avg_Rate is new Agpl.Average_Queue (Float);
   package Avg      is new Avg_Rate.Timed;

   type Stream_type is new Agpl.Streams.Filter.Stream_Type with
   record
      Buf_Out             : Agpl.Streams.Circular_Unbounded.Stream_Type;
      Throttle_In,
      Throttle_Out        : Agpl.Bandwidth_Throttle.Object_Access;

      Start               : Calendar.Time := Calendar.Clock;

      --  Statistics
      Total_Written       : Stream_Element_Count := 0;
      Total_Read          : Stream_Element_Count := 0;
      Avg_In              : Avg.Object_Access;
      Avg_Out             : Avg.Object_Access;
   end record;

end Agpl.Streams.Filter.Bandwidth_Throttle;
