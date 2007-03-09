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

with Agpl.Calendar.Format;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Chronos is

   use type Ada.Calendar.Time;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Elapsed : Duration := 0.0) is
   begin
      This.Start := Ada.Calendar.Clock - Elapsed;
   end Reset;

   ------------------------------------------------------------------------
   -- Elapsed                                                            --
   ------------------------------------------------------------------------
   function Elapsed (This : in Object) return Duration is
   begin
      return Ada.Calendar.Clock - This.Start;
   exception
      when E : others =>
         Log ("Chronos: " & Report (E), Error);
         Log ("Chronos: Why???", Error);
         return This.Elapsed; -- Try again!
   end Elapsed;

   ------------------------------------------------------------------------
   -- Image                                                              --
   ------------------------------------------------------------------------
   function Image (This : in Object) return String is
   begin
      return Agpl.Calendar.Format.Image (Elapsed (This));
   end Image;

   -----------
   -- Clock --
   -----------

   function Clock return Object is
   begin
      return (Start => Ada.Calendar.Clock);
   end Clock;

   -----------
   -- Value --
   -----------

   function Value (This : in Object) return Ada.Calendar.Time is
   begin
      return This.Start;
   end Value;

end Agpl.Chronos;
