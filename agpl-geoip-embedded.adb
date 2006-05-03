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
--  $Id: agpl-geoip-embedded.adb,v 1.1 2004/01/26 20:47:37 Jano Exp $

--  Uses the CSV database from www.maxmind.com

--  Allows initialization from an embedded database, if present.

with Aws.Resources;

package body Agpl.Geoip.Embedded is

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Call it before any other subroutine.
   -- Database is the name of the embedded resource (using Awsres).
   procedure Init (Database : in String; Success : out Boolean) is
      use Aws.Resources;
      F    : File_type;
      Line : String (1 .. 255);
      Last : Natural;
   begin
      Success := false; -- By now

      if not Aws.Resources.Is_regular_file (Database) then
         Success := false;
         return;  --  <-- Early exit, file not found.
      end if;

      Open (F, Database);
      while not End_of_file (F) loop
         Get_line (F, Line, Last);
         Parse_line (Line (Line'First .. Last));
      end loop;
      Close (F);

      Success := true;
   exception
      when others =>
         Close (F);
         raise;
   end Init;

end Agpl.Geoip.Embedded;
