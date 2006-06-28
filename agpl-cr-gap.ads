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

--  A gap is a segment.

with Agpl.Cv;
--  with Agpl.Dynamic_Vector;

package Agpl.Cr.Gap is

   pragma Preelaborate;

   type Kinds is (Occlusive, Out_of_range);
   --  Occlusive gaps are formed because some vertex prevents vision.
   --  Out of range gaps occur when the range sensor don't reach anything.

   type Object is private;

   type Object_Array is array (Positive range <>) of Object;

   function Create (P1, P2 : Cv.Point2D;
                    Kind   : Kinds := Occlusive) return Object;
   --  Create a Gap which starts at P1 and ends at P2

   function Get_Line (This : Object) return Cv.Line2D;

   function Get_Start (This : Object) return Cv.Point2D;

   function Get_End   (This : Object) return Cv.Point2D;

   function Get_Kind  (This : Object) return Kinds;

   --  Debug

   procedure Dump (This : Object);
   --  Dump description to stdout.

private

   type Object is record
      P1, P2 : Cv.Point2D;
      Line   : Cv.Line2D;
      Kind   : Kinds;
   end record;

   pragma Inline (Create, Get_Line, Get_Start, Get_End, Get_Kind);

end Agpl.Cr.Gap;
