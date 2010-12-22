 

--  Uses a internal circular buffer
--  Writting compresses, reading decompresses

--  Not much tested, use Deflate_Unbounded instead.

with Agpl.Streams.Circular;

with Zlib.Streams;

with Ada.Finalization;
with Ada.Streams;
use  Ada;

package Agpl.Streams.Deflate is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Write_Buffer_Is_Full : exception;
   Buffer_Not_Empty     : exception;

   Read_Buffer_Is_Full  : exception; -- Happens when the decompression rate is too high

   type Available_function is access
      function (This : access Ada.Streams.Root_stream_type'Class)
         return Natural;

   -- Helper always ready function, will return Natural'Last:
   function Always_Available (
      This : access Ada.Streams.Root_stream_type'Class) return Natural;
   -- Helper never available, will return zero:
   function Never_Available (
      This : access Ada.Streams.Root_stream_type'Class) return Natural;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type is new Ada.Streams.Root_stream_type with private;
   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- From is the stream to be pipelined.
   -- Avail_read/write are functions provided by the From stream if applicable.
   -- The percent free says when Ready_for_writing will return true, according
   -- to the internal buffer filling.
   -- It is the security margin for the underlying Z stream too.
   -- Theoretically, writing should always compress, but sometimes the data gets
   -- bigger.
   -- If some stream is not to be written/read, use the Never_Available argument
   -- appropriately.
   -- To ensure that no write fails because of internal buffer limitations (instead of
   -- a low pipe something along the line down), never write chunks of more than
   -- [Percent_Free] size, and check Ready_For_Writing before every write.
   -- Furthermore, the percent_free must be *higher* than the compression ratio
   -- of the incoming data or this will eventually fail.
   -- This is specially important in 'Write or 'Output calls who divide an array
   -- into calls to individual elements, since you coulnd't recover synch at that
   -- point even if you control the Buffer_Full exception. For example, if you do
   -- String'Output (blah, "blahblahblha"), you'll not know in which of the characters
   -- the write has failed.
   procedure Create (
      Stream       :    out Stream_type;
      From         : access Ada.Streams.Root_stream_type'Class; 
      Avail_read   : in     Available_function;
      Avail_write  : in     Available_function;
      Buffer       : in     Positive := 4096;
      Percent_free : in     Natural  := 20);

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read(
      Stream : in out Stream_type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   -- May raise some exception if not Ready_For_Writing
	procedure Write(
      Stream : in out Stream_type;
      Item   : in Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- Will fail if there is data pending to be flushed/written
   procedure Close (Stream : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Close_With_Abort                                                   --
   ------------------------------------------------------------------------
   -- Will always succeed, data pending will not be sent.
   procedure Close_With_Abort (Stream : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Soft_flush                                                         --
   ------------------------------------------------------------------------
   -- Do not flushes the Z streams, but the internal buffers to the 
   -- back stream.
   procedure Soft_flush (Stream : in out Stream_type);

   ------------------------------------------------------------------------
   -- Hard_flush                                                         --
   ------------------------------------------------------------------------
   -- Flushes all buffers, including the Z streams (reduces compression ratio)
   procedure Hard_flush (Stream : in out Stream_type);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data is ready to be read
   function Available_read (This : in Stream_type) return Natural;

   ------------------------------------------------------------------------
   -- Ready_for_writing                                                  --
   ------------------------------------------------------------------------
   -- You may need to call Soft_Flush/Hard_Flush until this becomes true
   function Ready_for_writing (This : in Stream_type) return Boolean;

   ------------------------------------------------------------------------
   -- Everything_Written                                                 --
   ------------------------------------------------------------------------
   -- True if no data is pending to be flushed/written in any internal buffer.
   -- Force this with Hard_Flush
   function Everything_Written (This : in Stream_Type) return Boolean;

   ------------------------------------------------------------------------
   -- Get_Write_Ratio                                                    --
   ------------------------------------------------------------------------
   -- Gives the compression ratio
   function Get_Write_Ratio (This : in Stream_Type) return Float;

   ------------------------------------------------------------------------
   -- Get_Read_Ratio                                                    --
   ------------------------------------------------------------------------
   -- Gives the compression ratio
   function Get_Read_Ratio (This : in Stream_Type) return Float;

private

   type Stream_finalizer (Parent : access Stream_type) is new
      Finalization.Limited_controlled with null record;

   procedure Finalize (This : in out Stream_finalizer);

   type Stream_type
   is new Ada.Streams.Root_stream_type with record
      Back    : Agpl.Streams.Stream_access;

      Zin,
      Zout    : Zlib.Streams.Stream_type;

      Buf_in  : Agpl.Streams.Circular.Stream_access;
      Buf_out : Agpl.Streams.Circular.Stream_access;

      Unflushed_ZOut  : Natural := 0; -- Amount in ZBuffer not flushed

      Percent_free : Natural;
      Buffer_size  : Natural;
      Min_free     : Natural;

      Avail_read,
      Avail_write  : Available_function;

      -- Buffers with data pending to be read/write to the CStreams
      Pending_write     : Agpl.Streams.Stream_element_array_access;
      Pending_write_pos : Ada.Streams.Stream_Element_Offset;

      -- Data for ratios
      Gained_Written       : Integer := 0;
      Uncompressed_Written : Natural := 0;
      Gained_Read          : Integer := 0;
      Uncompressed_Read    : Natural := 0;

      -- Finalizer:
      Finalize : Stream_finalizer (Stream_type'Access);
   end record;

   ------------------------------------------------------------------------
   -- Attempt_read                                                       --
   ------------------------------------------------------------------------
   -- Works on internal circular buffers
   -- Read as many data as possible restricted by availabilities.
   procedure Attempt_read (Stream : in out Stream_type);

   ------------------------------------------------------------------------
   -- Attempt_write                                                      --
   ------------------------------------------------------------------------
   -- Works on internal circular buffers
   -- Write as many data as possible restricted by availabilities.
   procedure Attempt_write (Stream : in out Stream_type);

end Agpl.Streams.Deflate;
