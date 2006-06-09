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
------------------------------------------------------------------------------

package Agpl.Endian is

   pragma Elaborate_body;

   --  Auxiliary types for later functions:
   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   type Byte_array is array (Integer range <>) of Byte;
   pragma Pack (Byte_array);

   --  Endingness of machine (determined at elaboration time):
   Little_Endian: Boolean;

   --  Convert an arbitrary long byte array in any endingness to integer
   --  May raise Constraint_error if array lengths exceedes integer capacity.
   function Convert (
      From        : Byte_array;
      Big_endian  : Boolean := True) return Integer;
   pragma Inline (Convert);

   function Convert_L (
      From        : Byte_array;
      Big_endian  : Boolean := True) return Long_Long_Integer;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Optimally returns the shortest possible array:
   --  I.e, 0 is returned as an empty array.
   function Convert (
      From        : Long_Long_Integer;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Size specified (in bytes):
   function Convert (
      From        : Long_Long_Integer;
      Size        : Natural;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Optimally returns the shortest possible array:
   --  I.e, 0 is returned as an empty array.
   function Convert (
      From        : Integer;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Size specified (in bytes):
   function Convert (
      From        : Integer;
      Size        : Natural;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts a byte array into a string of same bytes:
   function To_string (From : Byte_array) return String;

   --  Inverse of the previous:
   function To_byte_array (From : String) return Byte_array;

   --  Invert a byte_array order
   function Invert (From : Byte_array) return Byte_array;

private

   function Is_little_endian return Boolean;

end Agpl.Endian;
