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
--  $Id: agpl-geoip.adb,v 1.3 2004/02/03 22:52:08 Jano Exp $

with Agpl.Ip;
with Agpl.Strings.Fields;

with Text_io;

package body Agpl.Geoip is

   ----------------
   -- Parse_line --
   ----------------
   procedure Parse_line (Line : in String) is 
      subtype LLInteger is Long_long_integer;
      Lbound : Long_long_integer;
      NEntry : Ip_entry;
      Name   : Ustring;
      use Agpl.Strings.Fields;
      use IP_maps;
      use Code_maps;
   begin
      LBound             := LLInteger'Value (Select_field (Line, 6, '"'));
      NEntry.Upper_bound := LLInteger'Value (Select_field (Line, 8, '"'));
      NEntry.Code        := Select_field (Line, 10, '"');
      Name               := U (Select_field (Line, 12, '"'));

      Insert (IP_table,   LBound, NEntry);
      Insert (Code_table, NEntry.Code, Name);
   end Parse_line;

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Path to database
   procedure Init (Database : in String) is
      use Text_io;
      F    : File_type;
      Line : String (1 .. 255);
      Last : Natural;
   begin
      Open (F, Name => Database, Mode => In_file);
      while not End_of_file (F) loop
         Get_line (F, Line, Last);
         Parse_line (Line (Line'First .. Last));
      end loop;
      Close (F);
   exception
      when others =>
         if Is_open (F) then
            Close (F);
         end if;
         raise;
   end Init;

   ------------------------------------------------------------------------
   -- Country_code_by_addr                                               --
   ------------------------------------------------------------------------
   -- Addr in dot format
   function Country_code_from_addr (Addr : in String) return Country_code is
      use Ip_maps;
      N : Long_long_integer := Ip.To_number (Ip.Strip_port (Addr));
      I : Iterator_type := Lower_bound (Ip_table, N);
   begin
      if I /= Back (Ip_table) then
         if Key (I) = N then
            return Element (I).Code; -- <-------- EXACT MATCH
         else
            I := Pred (I);
         end if;
      else
         I := Pred (I);
      end if;

      if I = Back (Ip_table) then
         return Unknown_country;
      elsif Element (I).Upper_bound < N then
         return Unknown_country;
      else
         return Element (I).Code;
      end if;
   end Country_code_from_addr;

   ------------------------------------------------------------------------
   -- Country_name_from_addr                                             --
   ------------------------------------------------------------------------
   -- Addr in dot format, port optional
   function Country_name_from_addr (Addr : in String) return String is
   begin
      return Country_name_from_code (Country_code_from_addr (Addr));
   end Country_name_from_addr;

   ------------------------------------------------------------------------
   -- Country_name_from_code                                             --
   ------------------------------------------------------------------------
   -- Will work only for countries with existing IP ranges.
   -- "Unknown" otherwise.
   function Country_name_from_code (Code : in Country_code) return String is
      use Code_maps;
      I : Iterator_type := Find (Code_table, Code);
   begin
      if I /= Back (Code_table) then
         return S (Element (I));
      else
         return "Unknown";
      end if;
   end Country_name_from_code;
   
   ------------------------------------------------------------------------
   -- Flag_code_from_country_code                                        --
   ------------------------------------------------------------------------
   -- returns "unknown" instead of "??", else the code
   function Flag_code_from_country_code (Code : in Country_code) 
      return String
   is
   begin
      if Code = "??" then
         return "unknown";
      else
         return Code;
      end if;
   end Flag_code_from_country_code;

end Agpl.Geoip;
