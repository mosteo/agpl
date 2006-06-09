------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
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

package Agpl.Gdk is

   pragma Pure;

   type Float_Matrix is array (1 .. 3, 1 .. 3) of Float;
   type Float_Vector is array (1 .. 3) of Float;

   Identity : constant Float_Matrix :=
                ((1.0, 0.0, 0.0),
                 (0.0, 1.0, 0.0),
                 (0.0, 0.0, 1.0));
   Look_Up  : constant Float_Matrix :=
                ((0.0, -1.0, 0.0),
                 (1.0, 0.0, 0.0),
                 (0.0, 0.0, 1.0));
   --  To get easily a "from-behind" looking transform for things aligned
   --  along the X axis.

   function "*" (M : Float_Matrix; V : Float_Vector) return Float_Vector;
   --  Dot product

   function "**" (L, R : Float_Vector) return Float_Vector;
   --  Cross product

   function H (X : Float_Vector) return Float_Vector;
   --  Homogeneization (i.e. X (3) = 1.0).

   function Inverse (X : Float_Matrix) return Float_Matrix;

private

   pragma Inline ("*");

end Agpl.Gdk;
