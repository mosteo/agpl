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
--  $Id: agpl-ip.adb,v 1.1 2004/01/25 22:55:07 Jano Exp $

--  Utilities for IP conversions

with Agpl.Strings.Fields;

package body Agpl.Ip is

   ------------------------------------------------------------------------
   -- Strip_port                                                         --
   ------------------------------------------------------------------------
   --  Removes the port portion, if present
   function Strip_port (Address : in Any_address) return Any_address is
      use Agpl.Strings.Fields;
   begin
      return Select_field (Address, 1, ':');
   end Strip_port;

   ------------------------------------------------------------------------
   -- To_number                                                          --
   ------------------------------------------------------------------------
   --  Returns a dotted_address as long long integer
   --  w.x.y.z = w * 256**3 + x * 256**2 + y * 256**1 + z
   function To_number (Address : in Dotted_address) return Long_Long_Integer
   is
      Result : Long_Long_Integer := 0;
      use Agpl.Strings.Fields;
   begin
      for N in 1 .. 4 loop
         Result := Result + Long_Long_Integer'Value (
            Select_field (Address, N, '.')) * 256**(4 - N);
      end loop;

      return Result;
   end To_number;

end Agpl.Ip;
