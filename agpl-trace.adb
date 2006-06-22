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

--  Objects for tracing with disc dump optional.
--  This first implementation uses a protected object but not queuing.
--  Writing is done within the protected, which is theoretically illega.
--  Gnat's implementation of I/O allows it so in this first approach we'll
--  leave it like that.

package body Agpl.Trace is

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  In purpose, This can be null to allow the passing of Null_Object.
   procedure Log
     (This    : in     Object_Access;
      Text    : in     String;
      Level   : in     Levels;
      Section : in String := "") is
   begin
      if Enabled then
         if This /= null then
            Log (This.all, Text, Level, Section);
         end if;
      end if;
   end Log;

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  Logs to the default log object.
   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "") is
   begin
      if Enabled then
         Log (Get_Default_Tracer, Text, Level, Section);
      end if;
   end Log;

   ------------------------------------------------------------------------
   -- Default_Tracer                                                     --
   ------------------------------------------------------------------------
   Default_Tracer : Object_Access := null;
   pragma Atomic (Default_Tracer);

   ------------------------
   -- Get_Default_Tracer --
   ------------------------

   function Get_Default_Tracer return Object_Access is
   begin
      return Default_Tracer;
   end Get_Default_Tracer;

   ------------------------
   -- Set_Default_Tracer --
   ------------------------

   procedure Set_Default_Tracer (This : in Object_Access := Null_Object) is
   begin
      Default_Tracer := This;
   end Set_Default_Tracer;

end Agpl.Trace;
