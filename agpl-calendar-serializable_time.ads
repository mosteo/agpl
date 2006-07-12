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
--  $Id: agpl-calendar.ads,v 1.1 2004/01/21 21:05:52 Jano Exp $

with Ada.Calendar;
with Ada.Streams;

--  A Time can't be streamed to remote machines. This is an extension of
--  regular timestamps that replaces 'Read and 'Write with proper
--  serialization subprograms.

package Agpl.Calendar.Serializable_Time is

   --   pragma Elaborate_Body;

   type Object is new Ada.Calendar.Time;

   function "+" (This : in Object) return Ada.Calendar.Time;
   pragma Inline ("+");

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object);
   for Object'Read use Read;

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object);
   for Object'Write use Write;

private

   use Ada.Calendar;

   Epoch : constant Object := Time_Of (1970, 1, 1, 0.0);

end Agpl.Calendar.Serializable_Time;
