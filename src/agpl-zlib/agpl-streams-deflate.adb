 

with Zlib.Streams.Extra;

with Ada.Unchecked_Deallocation;
with Text_Io;

package body Agpl.Streams.Deflate is

   -- Helper always ready function, will return Natural'Last:
   function Always_available (
      This : access Ada.Streams.Root_stream_type'Class) return Natural
   is
      pragma Unreferenced (This);
   begin
      return Natural'Last;
   end Always_available;

   -- Helper never available, will return zero:
   function Never_Available (
      This : access Ada.Streams.Root_stream_type'Class) return Natural
   is
      pragma Unreferenced (This);
   begin
      return 0;
   end Never_Available;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- The percent free says when Ready_for_writing will return true, according
   -- to the internal buffer filling.
   procedure Create (
      Stream       :    out Stream_type;
      From         : access Ada.Streams.Root_stream_type'Class; 
      Avail_read   : in     Available_function;
      Avail_write  : in     Available_function;
      Buffer       : in     Positive := 4096;
      Percent_free : in     Natural  := 20)
   is
   begin
      Stream.Back          := Agpl.Streams.Stream_access (From);
      Stream.Avail_read    := Avail_read;
      Stream.Avail_write   := Avail_write;
      Stream.Buffer_size   := Buffer;
      Stream.Percent_free  := Percent_free;
      Stream.Min_Free      := Stream.Buffer_size * Stream.Percent_free / 100;
      Stream.Buf_in        := new Circular.Stream_type (Stream_Element_Offset (Buffer));
      Stream.Buf_out       := new Circular.Stream_type (Stream_Element_Offset (Buffer));

      Zlib.Streams.Create (
         Stream.Zin, 
         Zlib.Streams.Out_Stream,
         Zlib.Streams.Stream_Access (Stream.Buf_in),
         Back_compressed => false);
      Zlib.Streams.Create (
         Stream.Zout, 
         Zlib.Streams.Out_Stream,
         Zlib.Streams.Stream_Access (Stream.Buf_out),
         Back_compressed => true);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      Stream : in out Stream_type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      if Circular.Available_Read (Stream.Buf_In.all) = Natural'(0) or else
         Circular.Available_Read (Stream.Buf_In.all) < Natural'(Item'Length)
      then
         Attempt_read  (Stream);
      end if;
      Circular.Read (Stream.Buf_in.all, Item, Last);
      Stream.Uncompressed_Read := Stream.Uncompressed_Read + Integer (Last - Item'First + 1);
      Stream.Gained_Read       := Stream.Gained_Read       + Integer (Last - Item'First + 1);
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
	procedure Write(
      Stream : in out Stream_type;
      Item   : in Ada.Streams.Stream_Element_Array) 
   is
      Avail    : Ada.Streams.Stream_element_offset;
   begin
      if Stream.Unflushed_Zout >= Stream.Buffer_Size - Stream.Min_Free then
         --text_io.put_line ("Flushing because unflusehd is" & Stream.Unflushed_Zout'Img);
         Hard_Flush (Stream);
         --text_io.put_line ("Unflusehd is" & Stream.Unflushed_Zout'Img);
         --text_io.put_line ("Circular out avail read is" & 
         --   Integer'Image (Circular.Available_Read (Stream.Buf_Out.all)));
      end if;

      if not Ready_For_Writing (Stream) then
         raise Write_Buffer_Is_Full;
      end if;

      -- Text_Io.Put_Line ("Deflate.Write:" & Integer'Image (Item'Length));

      -- Write as much as possible, keep remaining in buffer:
      if Circular.Available_write (Stream.Buf_out.all) > Stream.Min_Free then
         Avail := Ada.Streams.Stream_Element_Offset'Min (
            Circular.Available_Write (Stream.Buf_Out.all) - Stream_Element_Offset (Stream.Min_Free),
            Item'Length);
         Zlib.Streams.Write (Stream.Zout, Item (Item'First .. Item'First + Avail - 1));
         Stream.Gained_Written       := Stream.Gained_Written       + Natural (Avail);
         Stream.Uncompressed_Written := Stream.Uncompressed_Written + Natural (Avail);
         Stream.UnFlushed_ZOut       := Stream.Unflushed_Zout       + Natural (Avail);
      else
         Avail := 0;
      end if;

      if Avail /= Item'Length then
         Stream.Pending_Write := new Ada.Streams.Stream_element_array'(
            Item (Item'First + Avail .. Item'Last));
         Stream.Pending_Write_Pos := Stream.Pending_Write'First;
      end if;

      Attempt_Write (Stream);

   end Write;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data is ready to be read
   function Available_Read (This : in Stream_Type) return Natural is
   begin
      return Circular.Available_Read (This.Buf_In.all);
   end Available_Read;

   ------------------------------------------------------------------------
   -- Ready_for_writing                                                  --
   ------------------------------------------------------------------------
   function Ready_for_Writing (This : in Stream_type) return Boolean is
   begin
      return 
         This.Pending_Write = null and then
         This.Unflushed_Zout < This.Buffer_Size - This.Min_Free and then
         Circular.Available_Write (This.Buf_Out.all) > This.Min_Free;
   end Ready_For_Writing;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Stream_Type) is
      procedure Free is new Unchecked_Deallocation (Circular.Stream_Type, Circular.Stream_Access);
   begin
      -- Free pendings:
      Free (This.Pending_Write);

      -- Close Sockets:
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

      Free (This.Buf_In);
      Free (This.Buf_Out);
   end Finalize;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Stream_finalizer) is
   begin
      Finalize (This.Parent.all);
   end Finalize;

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- Will fail if there is data pending to be flushed
   procedure Close (Stream : in out Stream_Type) is
   begin
      if Stream.UnFlushed_ZOut > 0 or else
         Stream.Pending_Write /= null or else
         Circular.Available_Read (Stream.Buf_Out.all) /= Natural'(0)
      then
         raise Buffer_Not_Empty;
      end if;
      Finalize (Stream);
   end Close;

   ------------------------------------------------------------------------
   -- Close_With_Abort                                                   --
   ------------------------------------------------------------------------
   -- Will always succeed, data pending will not be sent.
   procedure Close_With_Abort (Stream : in out Stream_Type) is
   begin
      Finalize (Stream);
   end Close_With_Abort;

   ------------------------------------------------------------------------
   -- Soft_flush                                                         --
   ------------------------------------------------------------------------
   -- Do not flushes the Z streams, but the internal buffers to the 
   -- back stream.
   procedure Soft_flush (Stream : in out Stream_type) is
      Avail    : Ada.Streams.Stream_Element_Offset;
   begin
      -- Reading
      Attempt_Read (Stream);

      -- Writting 
      Attempt_Write (Stream);
      if Stream.Pending_Write /= null and then
         Circular.Available_Write (Stream.Buf_Out.all) > Stream.Min_Free
      then
         Avail := Ada.Streams.Stream_Element_Offset'Min (
            Stream.Pending_Write'Last - Stream.Pending_Write_Pos + 1,
            Circular.Available_Write (Stream.Buf_Out.all) - Stream_Element_Offset (Stream.Min_Free));
         Zlib.Streams.Write (Stream.Zout, 
            Stream.Pending_Write (Stream.Pending_Write_Pos .. Stream.Pending_Write_Pos + Avail - 1));
         Stream.UnFlushed_ZOut := Stream.Unflushed_Zout + Natural (Avail);
         Stream.Pending_Write_Pos := Stream.Pending_Write_Pos + Avail;
         if Stream.Pending_Write_Pos > Stream.Pending_Write'Last then
            Free (Stream.Pending_Write);
         end if;
         -- Again
         Attempt_Write (Stream);
      end if;
   end Soft_Flush;

   ------------------------------------------------------------------------
   -- Hard_flush                                                         --
   ------------------------------------------------------------------------
   -- Flushes all buffers, including the Z streams (reduces compression ratio)
   procedure Hard_Flush (Stream : in out Stream_type) is
   begin
      Soft_Flush (Stream);
      Zlib.Streams.Flush (Stream.Zout);
      Stream.UnFlushed_ZOut := 0;
      Attempt_Write (Stream); -- Previous can cause new data to be buffered.
   end Hard_Flush;

   ------------------------------------------------------------------------
   -- Everything_Written                                                 --
   ------------------------------------------------------------------------
   -- True if no data is pending to be flushed/written in any internal buffer.
   -- Force this with Hard_Flush
   function Everything_Written (This : in Stream_Type) return Boolean is
   begin
      return 
         This.UnFlushed_ZOut = 0    and then 
         This.Pending_Write  = null and then
         Circular.Available_Read (This.Buf_Out.all) = Natural'(0);
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
   -- Attempt_read                                                       --
   ------------------------------------------------------------------------
   -- Read as many data as possible restricted by availabilities.
   procedure Attempt_Read (Stream : in out Stream_type) is
   begin
      if Circular.Available_Write (Stream.Buf_In.all) > Stream.Min_Free then
         declare
            Avail : constant Stream_Element_Offset := Stream_Element_Offset'Min (
               Circular.Available_Write (Stream.Buf_In.all) - Stream_Element_Offset (Stream.Min_free),
               Stream_Element_Offset (Stream.Avail_Read (Stream.Back)));
            Buf   : Ada.Streams.Stream_Element_Array (1 .. Avail);
            Last  : Ada.Streams.Stream_Element_Offset;
         begin
            if Buf'Length > 0 then
               --Text_Io.Put_Line ("Pushing data into zreader:" & Integer'Image (Buf'Length));
               Ada.Streams.Read (Stream.Back.all, Buf, Last);
               if Last >= Buf'First then
                  begin
                     Zlib.Streams.Write (Stream.Zin, Buf (Buf'First .. Last));
                  exception
                     when Constraint_Error =>
                        raise Read_Buffer_Is_Full;
                  end;
                  Stream.Gained_Read := Stream.Gained_Read - Natural (Last - Buf'First + 1);
                  -- We decrease since when uncompressed the size will be greater
               end if;
            end if;
         end;
      end if;
   end Attempt_Read;

   ------------------------------------------------------------------------
   -- Attempt_write                                                      --
   ------------------------------------------------------------------------
   -- Write as many data as possible restricted by availabilities.
   procedure Attempt_Write (Stream : in out Stream_type) is
      Avail : constant Stream_Element_Offset := Stream_Element_Offset'Min (
         Circular.Available_Read (Stream.Buf_Out.all),
         Stream_Element_Offset (Stream.Avail_Write (Stream.Back)));
      Buf   : Stream_Element_Array (1 .. Avail);
      Last  : Stream_Element_Offset;
   begin
      if Avail > 0 then
         Circular.Read (Stream.Buf_Out.all, Buf, Last);
         if Last >= Buf'First then
            Ada.Streams.Write (Stream.Back.all, Buf (Buf'First .. Last));
            Stream.Gained_Written := Stream.Gained_Written - Natural (Last - Buf'First + 1);
         end if;
      end if;
   end Attempt_Write;

end Agpl.Streams.Deflate;
