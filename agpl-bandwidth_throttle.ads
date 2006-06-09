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
--  $Id: adagio-bandwidth_manager.ads,v 1.7 2004/03/22 07:14:57 Jano Exp $

--  Class for bandwidth throttling.
--  Provides no arbitration; first come first served.

with Ada.Streams;
with Ada.Calendar;
use  Ada;

package Agpl.Bandwidth_Throttle is

   pragma Elaborate_Body;

   --  Bandwidth is the elements/second to allow
   --  Period is the period for feedback. Longer ones allow the use
   --    of unused bandwidth for more time. (In milliseconds)
   protected type Object (
      Bandwidth : Ada.Streams.Stream_Element_Count;
      Period    : Positive)
   is
      --  Gives the available bandwidth at this moment (real + extra):
      procedure Available (Bandwidth : out Ada.Streams.Stream_Element_Count);

      --  Issue a bandwidth petition. Awarded can be less that solicited.
      --  Awarded will never be more than Natural'Last / 2 so you can
      --    add Awarded + Awarded_Extra.
      --  Extra flag requests bandwidth from unused past cycles.
      --  Past cycles will faint in exponential fashion.
      procedure Commit (
         Desired : in     Ada.Streams.Stream_Element_Count;
         Awarded :    out Ada.Streams.Stream_Element_Count;
         Extra   : in     Boolean := False);

   private

      Gap      : Duration      := Duration (Period) / 1000.0;

      Remanent : Ada.Streams.Stream_Element_Count := 0; -- Remanent in this cycle
      Unused   : Ada.Streams.Stream_Element_Count := 0; -- Remanent from past cycles
      Last_Req : Calendar.Time                    := Calendar.Clock;

   end Object;

   type Object_access is access all Object;

   Unlimited : aliased Object (Ada.Streams.Stream_Element_Count'Last, 1000);

end Agpl.Bandwidth_Throttle;
