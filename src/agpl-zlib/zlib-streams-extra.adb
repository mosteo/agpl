 

package body Zlib.Streams.Extra is

   ------------------------------------------------------------------------
   -- Close_abort                                                        --
   ------------------------------------------------------------------------
   -- Closes a [decompressing] stream without flushing it
   procedure Close_abort (Stream : in out Stream_type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Stream_Element_Array, Buffer_Access);
   begin
      if
            Stream.Mode = Out_Stream or
            Stream.Mode = Duplex
      then
         begin
            Flush (Stream, Finish);
         exception
            when E : Zlib_error =>
               null;
--               Trace.Log ("Zlib.Streams.Close_abort [flush ok]: " &
--                  Trace.Report (E));
         end;
         begin
            Close (Stream.Writer);
         exception
            when E : Zlib_error =>
               null;
--               Trace.Log ("Zlib.Streams.Close_abort [close ok]: " &
--                  Trace.Report (E));
         end;
      end if;

      if
         Stream.Mode = In_Stream or
         Stream.Mode = Duplex
      then
         begin
            Close (Stream.Reader);
         exception
            when E : Zlib_error =>
               null;
--               Trace.Log (
--                  "Zlib.Streams.Close_abort [ok]: " & Trace.Report (E));
         end;
         Free  (Stream.Buffer);
      end if;
   end Close_abort;

end Zlib.Streams.Extra;
