 

--  Filter stream who allows to buffer for write or read some data. Just a
--  convenience to use Circular_Unbounded.

with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;

package Agpl.Streams.Buffered_Unbounded is

   pragma Elaborate_Body;

   --  These parameters affect new instances (a-la Float_Io).
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
   --  Will read from internal buffer
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   --  Will cache all data written
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Cleans everything inside
   procedure Reset (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Flush_Read                                                         --
   ------------------------------------------------------------------------
   --  Tries to read so many data from the back stream.
   procedure Flush_Read (
      This  : in out Stream_type;
      Count : in     Stream_Element_Count := Stream_Element_Count'Last);

   ------------------------------------------------------------------------
   -- Flush_Write                                                        --
   ------------------------------------------------------------------------
   --  Writes to the back buffer as many data as indicated or less if not as many is cached.
   procedure Flush_Write (
      This  : in out Stream_type;
      Count : in     Stream_Element_Count := Stream_Element_Count'Last);

   ------------------------------------------------------------------------
   -- Get_Buffered_Read_Count                                            --
   ------------------------------------------------------------------------
   --  Returns the amount of read buffered data.
   function Get_Buffered_Read_Count (This : in Stream_Type)
      return Stream_Element_Count;
   pragma Inline (Get_Buffered_Read_Count);

   ------------------------------------------------------------------------
   -- Get_Buffered_Write_Count                                           --
   ------------------------------------------------------------------------
   --  Returns the amount of written buffered data.
   function Get_Buffered_Write_Count (This : in Stream_Type)
      return Stream_Element_Count;
   pragma Inline (Get_Buffered_Write_Count);

private

   package Circular renames Agpl.Streams.Circular_Unbounded;

   type Stream_type (
      Back         : access Root_Stream_Type'Class)
   is new Agpl.Streams.Filter.Stream_Type with
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
