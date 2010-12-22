 

--  Filter stream which uses an Agpl.Bandwidth_Throttle to limit data rates.
--  Provides statistics about global data rate and "instantaneous" data rate.
--  The instantaneous data rate averaging can be configured.

with Agpl.Average_Queue;
with Agpl.Average_Queue.Timed;
with Agpl.Bandwidth_Throttle;
with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;
with Agpl.Types;

with Ada.Calendar;
use  Ada;

package Agpl.Streams.Bandwidth_Throttle is

   pragma Elaborate_Body;

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
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class);

   procedure Create (
      This              : in out Stream_Type;
      Back              : access Ada.Streams.Root_Stream_Type'Class;
      Throttle_In       : access Agpl.Bandwidth_Throttle.Object;
      Throttle_Out      : access Agpl.Bandwidth_Throttle.Object;
      Max_Buffer_Size   : in     Stream_Element_Count :=
                                    Stream_Element_Count'Last;
      Initial_Buffer    : in     Stream_Element_Count := 4096;
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
   -- Get_Buffered_Count                                                 --
   ------------------------------------------------------------------------
   --  Returns the amount of written buffered data.
   function Get_Buffered_Count (This : in Stream_Type)
      return Stream_Element_Count;

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

      Start               : Calendar.Time := Calendar.Clock;

      --  Statistics
      Total_Written       : Stream_Element_Count := 0;
      Total_Read          : Stream_Element_Count := 0;
      Avg_In              : Avg.Object_Access;
      Avg_Out             : Avg.Object_Access;
   end record;

end Agpl.Streams.Bandwidth_Throttle;
