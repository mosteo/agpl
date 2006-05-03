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

with Text_Io;

package body Agpl.Debug.Timer is

   procedure Finalize (This : in out Object) is
      use Ada.Calendar;
      use type Agpl.Trace.Object_Access;
      Elapsed : constant Duration := Clock - This.Start;
   begin
      if Elapsed > (Duration (This.Deadline) / 1000.0) then
         Text_Io.Put_Line ("Deadline failed");
         if This.Trace /= Agpl.Trace.Null_Object then
            Agpl.Trace.Log (This.Trace.all, 
               "Tracer: " & This.Id.all & ": Elapsed" &
               Duration'Image (Elapsed) & " > " & 
               Duration'Image (Duration (This.Deadline) / 1000.0), 
               Agpl.Trace.Error);
         end if;
         if This.Always_Raise or else This.Trace = Agpl.Trace.Null_Object then
            raise Deadline_Failed;
         end if;
      end if;
   end Finalize;

end Agpl.Debug.Timer;
