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

with Agpl.Types;

package Agpl.Conversions is

   ------------------------------------------------------------------------
   -- From_Hex                                                           --
   ------------------------------------------------------------------------
   --  A two char string
   function From_Hex (C : Types.Hex_Character) return Character;
   function From_Hex (S : String) return String;

   ------------------------------------------------------------------------
   -- To_Hex                                                             --
   ------------------------------------------------------------------------
   function To_Hex (C : in Character) return Types.Hex_Character;
   function To_Hex (S : in String) return String;
   function To_Hex (I : in Natural; Length : in Natural := 0) return String;
   --  Return the hex representation, 0-left-padded to use at most Length chars

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   --  Works as 'Img but removes leading/trailing spaces
   --  Performs rounding on floats
   function To_string (N : Integer) return String;
   function To_string (N : Float; Decimals : Natural := 2) return String;
   function To_string (N : Long_Long_Float; Decimals : Natural := 2) return String;

end Agpl.Conversions;
