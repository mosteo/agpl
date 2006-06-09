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
--  $Id: agpl-bmp-draw.ads,v 1.3 2004/03/03 00:06:06 Jano Exp $

--  Drawing inside BMPs

with Agpl.Types.Constants;

package Agpl.Bmp.Draw is

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   --  Fills the full BMP
   procedure Delete (
      This  : in out Object;
      Color : in     Types.RGB_Triplet := Types.Constants.Black);

   ------------------------------------------------------------------------
   -- Box                                                                --
   ------------------------------------------------------------------------
   --  Filled box
   procedure Box (
      This : in out Object;
      R1, C1,
      R2, C2 : in     Integer;
      Color  : in     Types.RGB_Triplet);

   ------------------------------------------------------------------------
   -- Circle                                                             --
   ------------------------------------------------------------------------
   procedure Circle (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Rad   : in     Natural;
      Color : in     Types.RGB_triplet;
      Fill  : in     Types.RGB_triplet);

   ------------------------------------------------------------------------
   -- Circunference                                                      --
   ------------------------------------------------------------------------
   procedure Circunference (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Rad   : in     Natural;
      Color : in     Types.RGB_triplet);

   ------------------------------------------------------------------------
   -- Line                                                               --
   ------------------------------------------------------------------------
   --  Bresenham algorithm for line drawing
   procedure Line (
      This   : in out Object;
      R1, C1,
      R2, C2 : in     Integer;
      Color  : in     Types.RGB_Triplet);

   ------------------------------------------------------------------------
   -- Plot                                                               --
   ------------------------------------------------------------------------
   --  Puts a single point
   procedure Plot (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Color : in     Types.RGB_Triplet) renames Set_pixel;

private

   ------------------------------------------------------------------------
   -- Circ                                                               --
   ------------------------------------------------------------------------
   procedure Circ (
      This   : in out Object;
      Row    : in     Integer;
      Col    : in     Integer;
      Rad    : in     Natural;
      Color  : in     Types.RGB_triplet;
      Fill   : in     Boolean := False;
      FColor : in     Types.RGB_triplet := Types.Constants.Black);

end Agpl.Bmp.Draw;
