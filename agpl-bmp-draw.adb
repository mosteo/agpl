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
--  $Id: agpl-bmp-draw.adb,v 1.3 2004/03/03 00:06:06 Jano Exp $

--  Drawing inside BMPs

package body Agpl.Bmp.Draw is

   ------------------------------------------------------------------------
   -- Box                                                                --
   ------------------------------------------------------------------------
   -- Filled box
   procedure Box (
      This : in out Object;
      R1, C1,
      R2, C2 : in     Integer;
      Color  : in     Types.RGB_Triplet) 
   is
   begin
      for Row in R1 .. R2 loop
         for Col in C1 .. C2 loop
            Set_pixel (This, Row, Col, Color);
         end loop;
      end loop;
   end Box;
      
   ------------------------------------------------------------------------
   -- Circ                                                               --
   ------------------------------------------------------------------------
   procedure Circ (
      This   : in out Object;
      Row    : in     Integer;
      Col    : in     Integer;
      Rad    : in     Natural;
      Color  : in     Types.RGB_triplet;
      Fill   : in     Boolean := false;
      FColor : in     Types.RGB_triplet := Types.Constants.Black)
   is
      procedure Plot_points (X, Y : in Integer) is
      begin
         Plot (This, Row + Y, Col + X, Color);
         Plot (This, Row + Y, Col - X, Color);
         Plot (This, Row - Y, Col + X, Color);
         Plot (This, Row - Y, Col - X, Color);
         Plot (This, Row + X, Col + Y, Color);
         Plot (This, Row + X, Col - Y, Color);
         Plot (This, Row - X, Col + Y, Color);
         Plot (This, Row - X, Col - Y, Color);
      end Plot_points;
      procedure Fill_scanline (X, Y : in Integer) is
      begin
         if Y > X then
            Line (This, 
               C1 => Col + Y - 1, R1 => Row + X, 
               C2 => Col - Y + 1, R2 => Row + X,
               Color => FColor);
            Line (This, 
               C1 => Col + Y - 1, R1 => Row - X, 
               C2 => Col - Y + 1, R2 => Row - X,
               Color => FColor);
            Line (This, 
               C1 => Col + X, R1 => Row - Y + 1, 
               C2 => Col + X, R2 => Row - (Rad / 2),
               Color => FColor);
            Line (This, 
               C1 => Col + X, R1 => Row + (Rad / 2), 
               C2 => Col + X, R2 => Row + Y - 1,
               Color => FColor);
            Line (This, 
               C1 => Col - X, R1 => Row - Y + 1, 
               C2 => Col - X, R2 => Row - (Rad / 2),
               Color => FColor);
            Line (This, 
               C1 => Col - X, R1 => Row + (Rad / 2), 
               C2 => Col - X, R2 => Row + Y - 1,
               Color => FColor);
         end if;
      end Fill_scanline;

      D : Integer := 3 - (2 * Rad);
      X : Integer := 0;
      Y : Integer := Rad;

   begin
      while X <= Y loop

         if Fill then
            Fill_scanline (X, Y);
         end if;

         Plot_points (X, Y);

         if D < 0 then
            D := D + (4 * X) + 6;
         else
            D := D + 4 * (X - Y) + 10;
            Y := Y - 1;
         end if;

         X := X + 1;

      end loop;
   end Circ;

   ------------------------------------------------------------------------
   -- Circle                                                             --
   ------------------------------------------------------------------------
   procedure Circle (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Rad   : in     Natural;
      Color : in     Types.RGB_triplet;
      Fill  : in     Types.RGB_triplet) is
   begin
      Circ (This, Row, Col, Rad, Color, true, Fill);
   end Circle;

   ------------------------------------------------------------------------
   -- Circunference                                                      --
   ------------------------------------------------------------------------
   procedure Circunference (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Rad   : in     Natural;
      Color : in     Types.RGB_triplet) is
   begin
      Circ (This, Row, Col, Rad, Color, false);
   end Circunference;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   -- Fills the full BMP
   procedure Delete (
      This  : in out Object; 
      Color : in     Types.RGB_Triplet := Types.Constants.Black)
   is
   begin
      for N in 1 .. This.Height loop
         for M in 1 .. This.Width loop
            Set_pixel (This, N, M, Color);
         end loop;
      end loop;
   end Delete;

   ------------------------------------------------------------------------
   -- Line                                                               --
   ------------------------------------------------------------------------
   -- Bresenham algorithm for line drawing
   procedure Line (
      This   : in out Object;
      R1, C1,
      R2, C2 : in     Integer;
      Color  : in     Types.RGB_Triplet)
   is
      InitialX : Integer := C1;
      InitialY : Integer := R1;

      Steep  : Boolean := false;
      DeltaX,
      DeltaY,
      Deltaa : Integer;
      StepX,
      StepY  : Integer;

      procedure Swap (L, R : in out Integer) is
         T : Integer := L;
      begin
         L := R;
         R := T;
      end Swap;

   begin
      DeltaX := Abs (C2 - C1);
      if C2 - C1 > 0 then
         StepX := 1;
      else
         StepX := -1;
      end if;
      DeltaY := Abs (R2 - R1);
      if R2 - R1 > 0 then
         StepY := 1;
      else
         StepY := -1;
      end if;
      if DeltaY > DeltaX then
         Steep := true;
         Swap (InitialX, InitialY);
         Swap (DeltaX, DeltaY);
         Swap (StepX, StepY);
      end if;
      Deltaa := (DeltaY * 2) - DeltaX;
      for Coord in 0 .. DeltaX - 1 loop
         if Steep then
            Plot (This, Row => InitialX, Col => InitialY, Color => Color);
         else
            Plot (This, Row => InitialY, Col => InitialX, Color => Color);
         end if;
         while Deltaa >= 0 loop
            InitialY := InitialY + StepY;
            Deltaa   := Deltaa - (DeltaX * 2);
         end loop;
         InitialX := InitialX + StepX;
         Deltaa   := Deltaa   + (DeltaY * 2);
      end loop;
      Plot (This, Row => R2, Col => C2, Color => Color);
   end Line;

end Agpl.Bmp.Draw;
