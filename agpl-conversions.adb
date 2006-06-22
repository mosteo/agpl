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

with Agpl.Strings;        use Agpl.Strings;

with Ada.Strings;
with Ada.Strings.fixed;

with Interfaces;

package body Agpl.Conversions is

   ------------------------------------------------------------------------
   -- From_Hex                                                           --
   ------------------------------------------------------------------------
   FHex : constant array (Character) of Interfaces.Unsigned_8 := (
         '0' => 0,  '1' => 1,  '2' => 2,  '3' => 3,  '4' => 4,
         '5' => 5,  '6' => 6,  '7' => 7,  '8' => 8,  '9' => 9,
         'a' => 10, 'A' => 10, 'b' => 11, 'B' => 11, 'c' => 12, 'C' => 12,
         'd' => 13, 'D' => 13, 'e' => 14, 'E' => 14, 'f' => 15, 'F' => 15,
         others => 255);
   function From_Hex (C : Types.Hex_Character) return Character is
      use Interfaces;
   begin
      return Character'Val (
         Shift_Left (FHex (C (C'First)), 4) or FHex (C (C'Last)));
   end From_hex;

   function From_Hex (S : String) return String is
      R : String (1 .. S'Length / 2);
      P : Natural := S'First;
   begin
      for N in R'Range loop
         R (N) := From_Hex (S (P .. P + 1));
         P     := P + 2;
      end loop;

      return R;
   end From_Hex;

   ------------------------------------------------------------------------
   -- To_Hex                                                             --
   ------------------------------------------------------------------------
   THex : constant array (0 .. 15) of Character := "0123456789ABCDEF";
   function To_Hex (C : Character) return Types.Hex_Character is
      use Interfaces;
      Byte : constant Unsigned_8 := Character'Pos (C);
   begin
      return "" &
         THex (Integer (Shift_right (Byte, 4))) &
         THex (Integer (Byte and 16#0f#));
   end To_Hex;

   function To_Hex (S : in String) return String is
      R : String (1 .. S'Length * 2);
      P : Natural := R'First;
   begin
      for N in S'Range loop
         R (P .. P + 1) := To_Hex (S (N));
         P              := P + 2;
      end loop;

      return R;
   end To_Hex;

   function To_Hex (I : in Natural; Length : in Natural := 0) return String is
      use Interfaces;
      Ui     : Unsigned_64 := Unsigned_64 (I);
      Result : String (1 .. 16);
      Pos    : Natural := Result'Last;
   begin
      while Ui /= 0 loop
         Result (Pos) := Thex (Integer (Ui and 16#0F#));
         Pos          := Pos - 1;
         Ui           := Shift_Right (Ui, 4);
      end loop;

      return Rpad (Result (Pos + 1 .. Result'Last), Length, '0');
   end To_Hex;

   ---------------
   -- To_String --
   ---------------

   function To_String (N : Integer) return String is
   begin
      return Trim (Integer'Image (N));
   end To_string;

   ------------------------------------------------------------------------
   -- Trim                                                               --
   ------------------------------------------------------------------------
   function Trim (This : in String) return String is
   begin
      return Ada.Strings.Fixed.Trim (This, Ada.Strings.Both);
   end Trim;

end Agpl.Conversions;
