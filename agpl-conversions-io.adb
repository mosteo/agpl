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

with Ada.Text_IO;

package body Agpl.Conversions.Io is

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------


   function To_String (N : Float; Decimals : Natural := 2) return String is
      package f_io is new Ada.Text_IO.Float_IO (Float);
      S    : String (1 .. 100);
   begin
      if Decimals > 0 then
         f_io.Put (S, N, Aft => Decimals, Exp => 0);
         return Trim (S);
      else
         return To_String (Integer (N));
      end if;
   end To_string;

   function To_String (N : Long_Long_Float; Decimals : Natural := 2) return String is
      package f_io is new Ada.Text_IO.Float_IO (Long_Long_Float);
      S    : String (1 .. 100);
   begin
      if Decimals > 0 then
         f_io.Put (S, N, Aft => Decimals, Exp => 0);
         return Trim (S);
      else
         return To_String (Integer (N));
      end if;
   end To_string;

   function To_Str (N : Real; Decimals : Natural := 2) return String is
      package F_Io is new Ada.Text_Io.Float_Io (Real);
      S    : String (1 .. 100);
   begin
      if Decimals > 0 then
         f_io.Put (S, N, Aft => Decimals, Exp => 0);
         return Trim (S);
      else
         return To_String (Integer (N));
      end if;
   end To_Str;

end Agpl.Conversions.Io;
