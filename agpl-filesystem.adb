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
--  $Id: agpl.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

with Agpl.Strings.Fields;

with Ada.Streams.Stream_Io;

package body Agpl.Filesystem is

   ------------------
   -- Ensure_Slash --
   ------------------

   function Ensure_Slash (This : in String; Separator : in Character := '/')
      return String is
   begin
      if This (This'Last) /= Separator then
         return This & Separator;
      else
         return This;
      end if;
   end Ensure_Slash;

   -----------------------
   -- Replace_Extension --
   -----------------------

   function Replace_Extension (This : in String; New_Ext : in String)
                               return    String
   is
   begin
      return Strings.Fields.String_Tail_Reverse (This, '.') & '.' & New_Ext;
   end Replace_Extension;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Name : String) return Ustring is
      Result : Ustring;
      use Ada.Streams.Stream_Io;
      F      : File_Type;
   begin
      Open (F, In_File, Name);
      declare
         Length : constant Natural := Natural (Size (F));
         Remain :          Natural := Length;
         Stream : constant Stream_Access := Ada.Streams.Stream_Io.Stream (F);
      begin
         while Remain > 0 loop
            declare
               Chunk : String (1 .. Natural'Min (Remain, 1000));
            begin
               String'Read (Stream, Chunk);
               Asu.Append (Result, Chunk);
               Remain := Remain - Chunk'Length;
            end;
         end loop;
      end;
      Close (F);
      return Result;
   exception
      when others =>
         Close (F);
         raise;
   end Read_File;

end Agpl.Filesystem;
