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

--  Root package for Computer Vision

package Agpl.Cv is

   pragma Pure;

   type Float_Array is array (Positive range <>) of Float;

   type Float_Array_3 is new Float_Array (1 .. 3);

   function Inner_Product (L, R : Float_Array_3) return Float;

   function "*"           (L, R : Float_Array_3) return Float renames Inner_Product;

   function Cross_Product (L, R : Float_Array_3) return Float_Array_3;
   --  The types aren't that important because of the duality principle.
   --  This is arguable but for convenience will have it like this.

   function "**"          (L, R : Float_Array_3) return Float_Array_3
                           renames Cross_Product;

   type Point2D is new Float_Array_3;
   type Line2D  is new Float_Array_3;

   function Distance      (L, R : Point2D) return Float;
   --  Euclidean distance between two points.

   function Distance      (Line : Line2D; Point : Point2D) return Float;
   --  Shortest distance from a point to a line.

   function Signed_Distance (Line : Line2D; Point : Point2D) return Float;
   --  Distance is signed to mean at what side the point is of the line.
   --  If line is A x B, being above AB is positive, below is negative.

   function Normalize     (Point : Point2D) return Point2D;
   --  Ensure that P (3) = 1.0, unless it is 0.0

private

   pragma Inline (Inner_Product, Cross_Product, Distance, Normalize);

end Agpl.Cv;
