 

package Zlib.Streams.Extra is

   ------------------------------------------------------------------------
   -- Close_abort                                                        --
   ------------------------------------------------------------------------
   -- Closes a [decompressing] stream without flushing it
   procedure Close_abort (Stream : in out Stream_type);

end Zlib.Streams.Extra;
