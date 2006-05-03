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

with Ada.Numerics;

--  Values of general use across Agpl

package Agpl.Types.Constants is

   pragma Pure;

   ----------
   -- Math --
   ----------
   Pi         : constant := Ada.Numerics.Pi;
   Pi_2       : constant := Pi / 2.0;
   Pi_4       : constant := Pi / 4.0;
   Two_Pi     : constant := 2.0 * Pi;
   Three_Pi_4 : constant := Pi * 3.0 / 4.0;
   Three_Pi_2 : constant := Pi * 3.0 / 2.0;

   --------------
   -- Sections --
   --------------
   --  For debugging
   CR  : constant String := "Cooperative robotics";
   CV  : constant String := "Computer vision";
   HTN : constant String := "Hierarchical task networks";

   ------------------------------------------------------------------------
   -- Colours                                                            --
   ------------------------------------------------------------------------
   --  Primary
   White  : constant RGB_Triplet := (255, 255, 255);
   Black  : constant RGB_Triplet := (  0,   0,   0);
   Red    : constant RGB_Triplet := (255,   0,   0);
   Green  : constant RGB_Triplet := (  0, 255,   0);
   Blue   : constant RGB_Triplet := (  0,   0, 255);

   --  Secondary
   Yellow : constant RGB_Triplet := (255, 255,   0);

   --  Web
   Navy      : constant RGB_Triplet := (  0,   0, 128);
   Gray      : constant RGB_Triplet := (128, 128, 128);
   Silver    : constant RGB_Triplet := (192, 192, 192);
   Gainsboro : constant RGB_Triplet := (220, 220, 220);

   Soft_Green     : constant RGB_Triplet := (  0, 192,   0);
   Middle_Green   : constant RGB_Triplet := (  0, 128,   0);
   Dark_Green     : constant RGB_Triplet := (  0, 096,   0);

   Soft_Red       : constant RGB_Triplet := (192,   0,   0);
   Middle_Red     : constant RGB_Triplet := (128,   0,   0);
   Dark_Red       : constant RGB_Triplet := (096,   0,   0);
   Crimson        : constant RGB_Triplet := (128,   0,   0);

end Agpl.Types.Constants;
