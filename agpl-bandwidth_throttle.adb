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
--  $Id: adagio-bandwidth_manager.adb,v 1.8 2004/01/21 21:05:48 Jano Exp $

package body Agpl.Bandwidth_Throttle is

   use Ada.Streams;

   Safe_Maximum : constant := (Stream_Element_Count'Last - 1) / 2;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Object is

      procedure Refresh;
      pragma Inline (Refresh);

      ---------------
      -- Available --
      ---------------
      --  Gives the available bandwidth at this moment (real + extra):
      procedure Available (Bandwidth : out Stream_Element_Count) is
      begin
         Refresh;
         begin
            Bandwidth := Remanent + Unused;
         exception
            when Constraint_Error =>
               Bandwidth := Stream_Element_Count'Last;
         end;
      end Available;

      ------------
      -- Commit --
      ------------
      --  Issue a bandwidth petition. Awarded can be less that solicited.
      --  Extra flag requests bandwidth from unused past cycles.
      procedure Commit (
         Desired : in     Stream_Element_Count;
         Awarded :    out Stream_Element_Count;
         Extra   : in     Boolean := False) is
      begin
         Refresh;
         if Extra then
            Awarded  := Stream_Element_Count'Min (
               Desired, Stream_Element_Count'Min (Unused, Safe_Maximum));
            Unused   := Unused - Awarded;
         else
            Awarded  := Stream_Element_Count'Min (
               Desired, Stream_Element_Count'Min (Remanent, Safe_Maximum));
            Remanent := Remanent - Awarded;
         end if;
      end Commit;

      -------------
      -- Refresh --
      -------------
      --  Recomputes if necessary the BW available
      procedure Refresh is
         use Ada.Calendar;
         Elapsed : constant Duration := Clock - Last_Req;
      begin
         --  Check for too many time elapsed:
         if Elapsed >= Gap then
            --  Keep unused from past cycles:
            Unused := Remanent;
            --  Update remanent:
            begin
               Remanent := Stream_Element_Count (Float'Floor (Float (Bandwidth) * Float (Gap)));
            exception
               when Constraint_Error =>
                  Remanent := Stream_Element_Count'Last;
            end;
            --  Update clock:
            declare
               Gaps : constant Natural :=
                  Natural (Float'Floor (Float (Elapsed) / Float (Gap)));
            begin
               Last_Req := Last_Req + Gap * Duration (Gaps);
            end;
         end if;
      end Refresh;

   end Object;

end Agpl.Bandwidth_Throttle;
