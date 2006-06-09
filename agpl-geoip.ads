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
--  $Id: agpl-geoip.ads,v 1.3 2004/02/03 22:52:08 Jano Exp $

--  Uses the CSV database from www.maxmind.com

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Ordered_Maps;

package Agpl.Geoip is

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------
   subtype Country_code is String (1 .. 2);

   Unknown_country : constant Country_code := "??";

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   --  Call it before any other subroutine.
   --  Path to database.
   procedure Init (Database : in String);

   ------------------------------------------------------------------------
   -- Country_code_from_addr                                             --
   ------------------------------------------------------------------------
   --  Addr in dot format, port optional
   function Country_code_from_addr (Addr : in String) return Country_code;

   ------------------------------------------------------------------------
   -- Country_name_from_addr                                             --
   ------------------------------------------------------------------------
   --  Addr in dot format, port optional
   function Country_name_from_addr (Addr : in String) return String;

   ------------------------------------------------------------------------
   -- Country_name_from_code                                             --
   ------------------------------------------------------------------------
   --  Will work only for countries with existing IP ranges.
   --  "Unknown" otherwise.
   function Country_name_from_code (Code : in Country_code) return String;

   ------------------------------------------------------------------------
   -- Flag_code_from_country_code                                        --
   ------------------------------------------------------------------------
   --  returns "unknown" instead of "??", else the code
   function Flag_code_from_country_code (Code : in Country_code)
      return String;
   pragma Inline (Flag_code_from_country_code);

private

   type Ip_entry is record
      Upper_bound : Long_Long_Integer; -- IP
      Code        : String (1 .. 2);   -- Country code
   end record;

   package IP_maps is new Ada.Containers.Ordered_Maps (
      Long_Long_Integer, Ip_entry, "<", "=");

   IP_table : IP_maps.Map;

   package Code_maps is new Ada.Containers.Ordered_Maps (
      Country_code, Ustring, "<", Agpl.Types.Ustrings.ASU."=");

   Code_table : Code_maps.Map;

   --  Parses a line of CSV and adds it to the tables
   procedure Parse_line (Line : in String);

end Agpl.Geoip;
