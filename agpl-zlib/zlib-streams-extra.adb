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
--  $Id: zlib-streams-extra.adb,v 1.5 2004/01/21 21:05:48 Jano Exp $

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
