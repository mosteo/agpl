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
--  $Id: agpl-delayer.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

with Agpl.Trace;

with Ada.Calendar;
with Ada.Finalization;

package Agpl.Debug.Timer is

   pragma Elaborate_Body;

   Deadline_Failed : exception;

   Unknown_Context : aliased String := "Unknown context";

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  Declare at the context you want to ensure it lasts less than Deadline.
   --  If deadline is missed, upon exit of the context a trace will be generated.
   --  If Trace = Null_Object, then instead a Deadline_Failed will be raised.
   --  If Always_Raise, the exception will be raised even if the trace is generated.
   --  Give deadline in milliseconds.
   type Object (
      Id           : access String            := Unknown_Context'Access;
      Trace        : Agpl.Trace.Object_Access := Agpl.Trace.Null_Object;
      Deadline     : Natural                  := 10_000;
      Always_Raise : Boolean                  := False)
   is limited private;

private

   type Object (
      Id           : access String            := Unknown_Context'Access;
      Trace        : Agpl.Trace.Object_Access := Agpl.Trace.Null_Object;
      Deadline     : Natural                  := 10_000;
      Always_Raise : Boolean                  := False) is
   new Ada.Finalization.Limited_Controlled with
   record
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

   procedure Finalize (This : in out Object);

end Agpl.Debug.Timer;
