 

with Zlib.Streams.Extra;

-- with Text_Io;

package body Agpl.Streams.Filter.Deflate_Unbounded is

   procedure Initialize (This : in out Stream_Type) is
   begin
      Zlib.Streams.Create (
         This.Zin, 
         Zlib.Streams.Out_Stream,
         This.Buf_In'Unchecked_Access,
         Back_compressed => false);
      Zlib.Streams.Create (
         This.Zout, 
         Zlib.Streams.Out_Stream,
         This.Buf_Out'Unchecked_Access,
         Back_compressed => true);
   end Initialize;

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
      Lazy              : in     Boolean              := true) 
   is
   begin
      This.Back := Agpl.Streams.Stream_Access (Back);

      Circular.Create (
         This.Buf_In,
         Max_Memory_Usage,
         Initial_Size,
         Grow_Factor,
         Lazy);
      Circular.Create (
         This.Buf_Out,
         Max_Memory_Usage,
         Initial_Size,
         Grow_Factor,
         Lazy);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
      Avail : constant Stream_Element_Count := Available_Read (This);
   begin
      -- Get data from compressed stream and uncompress:
      if Avail < Item'Length then
         Attempt_Read  (This, Item'Length - Avail);
      end if;
--      Text_Io.Put_Line ("Req:" & Integer'Image (Item'Length) & " Av:" & 
--         Stream_Element_Count'Image (Available_Read (This)));

      -- Return required data:
      Circular.Read (This.Buf_In, Item, Last);
      This.Uncompressed_Read := This.Uncompressed_Read + Integer (Last - Item'First + 1);
      This.Gained_Read       := This.Gained_Read       + Integer (Last - Item'First + 1);
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
	procedure Write(
      This   : in out Stream_type;
      Item   : in     Ada.Streams.Stream_Element_Array) 
   is
   begin
      -- Write to Z out stream:
      Zlib.Streams.Write (This.Zout, Item);
      This.Gained_Written       := This.Gained_Written       + Item'Length;
      This.Uncompressed_Written := This.Uncompressed_Written + Item'Length;
      This.UnFlushed_ZOut       := This.Unflushed_Zout       + Item'Length;

      -- Send compressed to back buffer:
      Attempt_Write (This);
   end Write;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data is ready to be read
   function Available_Read (This : in Stream_Type) return Stream_Element_Count is
   begin
      return Circular.Available_Read (This.Buf_In);
   end Available_Read;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   -- Returns Max_Memory_Usage
   function Available_Write (This : in Stream_Type) 
      return Stream_Element_Count is
   begin
      return Circular.Available_Write (This.Buf_Out);
   end Available_Write;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Stream_Type) is
   begin
      -- Close Z:
      begin
         Zlib.Streams.Extra.Close_Abort (This.Zout);
      exception
         when others =>
            raise;
      end;
      begin
         Zlib.Streams.Extra.Close_Abort (This.Zin);
      exception
         when others =>
            raise;
      end;

      -- Reset circular buffers (shrink buffers):
      Circular.Reset (This.Buf_In);
      Circular.Reset (This.Buf_Out);
   end Finalize;

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- Will fail if there is data pending to be flushed
   procedure Close (This : in out Stream_Type) is
   begin
      if This.UnFlushed_ZOut > 0 or else
         Circular.Available_Read (This.Buf_Out) /= Natural'(0)
      then
         raise Pending_Data;
      end if;
      Finalize (This);
   end Close;

   ------------------------------------------------------------------------
   -- Close_With_Abort                                                   --
   ------------------------------------------------------------------------
   -- Will always succeed, data pending will not be sent.
   procedure Close_With_Abort (This : in out Stream_Type) is
   begin
      Finalize (This);
   end Close_With_Abort;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   -- Cleans everything inside, including stats.
   procedure Reset (This : in out Stream_Type) is
   begin
      Finalize   (This);
      Initialize (This);
      This.Gained_Written       := 0; 
      This.Uncompressed_Written := 0;
      This.Gained_Read          := 0;
      This.Uncompressed_Read    := 0;
   end Reset;

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   -- Tries to read at least this many data from the compressed stream to
   -- allow making some uncompressed data available
   procedure Prefetch (
      This : in out Stream_Type; Count : in Stream_Element_Count)
      renames Single_Read;

   ------------------------------------------------------------------------
   -- Fetch                                                              --
   ------------------------------------------------------------------------
   -- Read from back until @Count@ uncompressed data is available.
   procedure Fetch (
      This : in out Stream_Type; Count : in Stream_Element_Count)
      renames Attempt_Read;

   ------------------------------------------------------------------------
   -- Flush                                                              --
   ------------------------------------------------------------------------
   procedure Flush (This : in out Stream_type) is
   begin
      Zlib.Streams.Flush (This.Zout);
      This.UnFlushed_ZOut := 0;
      Attempt_Write (This); -- Previous can cause new data to be buffered.
   end Flush;

   ------------------------------------------------------------------------
   -- Everything_Written                                                 --
   ------------------------------------------------------------------------
   -- True if no data is pending to be flushed/written in any internal buffer.
   -- Force this with Hard_Flush
   function Everything_Written (This : in Stream_Type) return Boolean is
   begin
      return 
         This.UnFlushed_ZOut = 0    and then 
         Circular.Available_Read (This.Buf_Out) = Natural'(0);
   end Everything_Written;

   ------------------------------------------------------------------------
   -- Get_Write_Ratio                                                    --
   ------------------------------------------------------------------------
   -- Gives the compression ratio
   -- Gained / Written
   function Get_Write_Ratio (This : in Stream_Type) return Float is
   begin
      if This.Uncompressed_Written = 0 then
         return 0.0;
      else
         return Float (This.Gained_Written) / Float (This.Uncompressed_Written);
      end if;
   end Get_Write_Ratio;

   ------------------------------------------------------------------------
   -- Get_Read_Ratio                                                    --
   ------------------------------------------------------------------------
   -- Gives the compression ratio
   -- Gained / Read
   function Get_Read_Ratio (This : in Stream_Type) return Float is
   begin
      if This.Uncompressed_Read = 0 then
         return 0.0;
      else
         return Float (This.Gained_Read) / Float (This.Uncompressed_Read);
      end if;
   end Get_Read_Ratio;

   ------------------------------------------------------------------------
   -- Single_Read                                                        --
   ------------------------------------------------------------------------
   -- Read as requested from the compressed stream. No expectations on un-
   -- compressed data will be tried to meet.
   procedure Single_Read (
      This   : in out Stream_type;
      Count  : in     Stream_Element_Count) 
   is
      Buff : Stream_Element_Array (1 .. Count);
      Last : Stream_Element_Offset;
   begin
      Ada.Streams.Read (This.Back.all, Buff, Last);
      if Last >= Buff'First then
         Zlib.Streams.Write (This.Zin, Buff (Buff'First .. Last));
         This.Gained_Read := This.Gained_Read - Natural (Last - Buff'First + 1);
         -- We decrease since when uncompressed the size will be greater
      end if;
   end Single_Read;

   ------------------------------------------------------------------------
   -- Attempt_read                                                       --
   ------------------------------------------------------------------------
   procedure Attempt_Read (
      This   : in out Stream_type;
      Count  : in     Stream_Element_Count) 
   is
      Buff : Stream_Element_Array (1 .. Count);
      Last : Stream_Element_Offset;
      Prev : Stream_Element_Count := Available_Read (This);
      Next : Stream_Element_Count;
      Got  : Stream_Element_Count := 0;
   begin
      loop
         Ada.Streams.Read (This.Back.all, Buff, Last);
         if Last >= Buff'First then
            Zlib.Streams.Write (This.Zin, Buff (Buff'First .. Last));
            This.Gained_Read := This.Gained_Read - Natural (Last - Buff'First + 1);
            -- We decrease since when uncompressed the size will be greater
            
            Next := Available_Read (This);
            Got  := Got + Next - Prev;
            Prev := Next;
--            Text_Io.Put_Line ("Got:" & Got'Img);
         end if;
         exit when Last < Buff'First or else Got >= Count;
      end loop;
--      Text_Io.Put_Line ("Got total:" & Got'Img);
   end Attempt_Read;

   ------------------------------------------------------------------------
   -- Attempt_write                                                      --
   ------------------------------------------------------------------------
   -- Write as many data as possible restricted by availabilities.
   procedure Attempt_Write (This : in out Stream_type) is
      Avail : constant Stream_Element_Offset := Circular.Available_Read (This.Buf_Out);
      Buff  : Stream_Element_Array (1 .. Avail);
      Last  : Stream_Element_Offset;
   begin
      if Avail > 0 then
         Circular.Read (This.Buf_Out, Buff, Last);
         Ada.Streams.Write (This.Back.all, Buff (Buff'First .. Last));
         This.Gained_Written := This.Gained_Written - Natural (Last - Buff'First + 1);
      end if;
   end Attempt_Write;

end Agpl.Streams.Filter.Deflate_Unbounded;
