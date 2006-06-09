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
--  $Id: agpl-bmp.ads,v 1.3 2004/02/29 20:36:41 Jano Exp $

--  Packages for work with BMP files

with Agpl.Streams;
with Agpl.Types;

with Interfaces;

with Ada.Finalization;
with Ada.Streams;

package Agpl.Bmp is

   --  Mime_type
   Mime_type : constant String := "image/bmp";

   --  Exception if drawing out of bounds:
   Coordinates_out_of_bounds   : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  A Bmp object. Only 24bpp, uncompressed are valid ATM.
   type Object is tagged private;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This : in out Object; Width : in Positive; Height : in Positive);

   ------------------------------------------------------------------------
   -- Get_pixel                                                          --
   ------------------------------------------------------------------------
   function Get_pixel (
      This   : in Object;
      Row,
      Column : in Integer) return Types.Rgb_triplet;

   ------------------------------------------------------------------------
   -- Set_pixel                                                          --
   ------------------------------------------------------------------------
   procedure Set_pixel (
      This   : in out Object;
      Row,
      Column : in     Integer;
      Rgb    : in     Types.Rgb_triplet);

   ------------------------------------------------------------------------
   -- Set_checking                                                       --
   ------------------------------------------------------------------------
   --  If drawing outbounds, we can get an error or silent discarding:
   procedure Set_checking (This : in out Object; Check : in Boolean := True);

   ------------------------------------------------------------------------
   -- Get_stream                                                         --
   ------------------------------------------------------------------------
   --  Returns a stream with a valid BMP representation (not the pixel matrix).
   function Get_stream (This : in Object)
      return Ada.Streams.Stream_element_array;

private

   pragma Inline (Get_pixel, Set_pixel);

   --  Data types
   type Short_int is new Interfaces.Integer_16;
   type Int       is new Interfaces.Integer_32;

   type Object is new Ada.Finalization.Controlled with record
      --  Pixels
      Data   : Agpl.Streams.Stream_element_array_access;
      --  Dimensions
      Width  : Positive;
      Height : Positive;
      --  Checking
      Checking : Boolean := True;
   end record;

   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);
   procedure Finalize   (This : in out Object);

end Agpl.Bmp;
