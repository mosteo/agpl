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
--  $Id: agpl-calendar-format.adb,v 1.2 2004/02/03 22:52:08 Jano Exp $

with Agpl.Strings;

with Ada.Strings;
with Ada.Strings.Fixed;

with GNAT.Calendar;

with Text_IO;

package body Agpl.Calendar.Format is

   -----------------------------------
   -- Image                         --
   -----------------------------------
   --  Returns a beautified duration in hours, minutes, seconds, milliseconds
   function Image (D : Duration) return String is
      S  : String (1 .. 17) :=
        (3      => 'h',
         7      => 'm',
         11     => 's',
         16     => 'm',
         17     => 's',
         others => ' ');
      S2 : String (1 .. 10);
      package Int_io is new Text_IO.Integer_IO (Integer);
      use Int_io;
      Seconds : constant Integer := Integer (Float'Floor (Float (D)));
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Put (S (5 .. 6), (Seconds rem 3600) / 60);
      Put (S (9 .. 10), Seconds rem 60);
      Put (S (13 .. 15),
           Integer (1000.0 * D - Duration (Seconds * 1000)));
      if Seconds / 3600 > 99 then
         Put (S2, Seconds / 3600);
         return Trim (S2, Both) & "h " & S (5 .. S'Last);
      else
         Put (S (1 .. 2), Seconds / 3600);
         return S;
      end if;
   end Image;

   ------------------------------------------------------------------------
   -- Hour                                                               --
   ------------------------------------------------------------------------
   --  Returns as a string hh:mm:ss
   function Hour (T : in Ada.Calendar.Time) return String is
      use Agpl.Strings;
   begin
      return Rpad (To_String (GNAT.Calendar.Hour (T)), 2, '0') &
             ":" &
             Rpad (To_String (GNAT.Calendar.Minute (T)), 2, '0') &
             ":" &
             Rpad (To_String (GNAT.Calendar.Second (T)), 2, '0');
   end Hour;

   ------------------------------------------------------------------------
   -- Timestamp                                                          --
   ------------------------------------------------------------------------
   function Timestamp
     (h    : Ada.Calendar.Time := Ada.Calendar.Clock)
      return String
   is
      use Ada.Calendar;
      package Int_Io is new Text_IO.Integer_IO (Integer);
      use Int_Io;
      s   : String (1 .. 11) := (3 => ':', 6 => ':', 9 => '.', others => '0');
      d   : Day_Duration;
      seg : Integer;
   begin
      d   := Seconds (h);
      seg := Integer (d * 100);
      Put (s (1 .. 2), seg / (60 * 60 * 100));
      Put (s (4 .. 5), (seg rem 360000) / 6000);
      Put (s (7 .. 8), (seg rem 6000) / 100);
      Put (s (10 .. 11), seg rem 100);
      for i in  s'Range loop
         if s (i) = ' ' then
            s (i) := '0';
         end if;
      end loop;
      return s;
   exception
      when others =>
         return "??:??:??.??";
   end Timestamp;

   function Datestamp (H         : in Ada.Calendar.Time := Ada.Calendar.Clock;
                       Separator : in Character := '/')
                       return String
   is
      --  yyyy.mm.dd
      use Ada.Calendar;
      package Int_Io is new Text_IO.Integer_IO (Integer);
      use Int_Io;
      s   : String (1 .. 10) := (5 => Separator, 8 => Separator, others => <>);
   begin
      Put (s (1 .. 4), Year (H));
      Put (s (6 .. 7), Month (H));
      Put (s (9 .. 10), Day (H));
      for i in  s'Range loop
         if s (i) = ' ' then
            s (i) := '0';
         end if;
      end loop;
      return s;
   exception
      when others =>
         return "????" & Separator & "??" & Separator & "?";
   end Datestamp;

end Agpl.Calendar.Format;
