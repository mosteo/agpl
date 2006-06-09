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

--  Some stock figures to draw easily.

with Agpl.Cr.Gap;
with Agpl.Gdk.Drawer;

with Gdk.Gc; use Gdk.Gc;

package Agpl.Gdk.Drawer_Figures is

   pragma Elaborate_Body;

   procedure Draw_Arrow (X1, Y1, X2, Y2 : in     Float;
                         Size           : in     Float;
                         Gc             : in     Gdk_Gc;
                         Draw           : in out Drawer.Object);
   --  Draw a segment with arrow head at X2, Y2 of @Size@ size.

   procedure Draw_Gap (X    : in     Cr.Gap.Object;
                       Gc   : in     Gdk_Gc;
                       Draw : in out Drawer.Object);
   --  A gap seen as a segment.

   procedure Draw_Grid_Points
     (X1, Y1, X2, Y2 : in     Float;
      Gc             : in     Gdk_Gc;
      Draw           : in out Drawer.Object;
      Every_X        : in     Float := 1.0;
      Every_Y        : in     Float := 1.0);
   --  Draw a grid made of points (origin at 0, 0).

   procedure Draw_Plus (X, Y : in     Float;
                        Gc   : in     Gdk_Gc;
                        Draw : in out Drawer.Object;
                        Size : in     Float := 1.0);
   --  A cross sign.

   procedure Draw_Robot (X, Y, A : in     Float;
                         Gc      : in     Gdk_Gc;
                         Draw    : in out Drawer.Object;
                         Size    : in     Float := 0.5);
   --  A typical rectangular-with-arrow shaped robot.

   procedure Draw_Segment (X1, Y1, X2, Y2 : in     Float;
                           Gc             : in     Gdk_Gc;
                           Draw           : in out Drawer.Object);
   --  Drawing of a segment in just a call.

   procedure Draw_Vector (X, Y : in Float;
                          A, L : in Float;
                          Gc   : in Gdk_Gc;
                          Draw : in out Drawer.Object);
   --  Draw a oriented line starting at point XY, with angle A and length L.

end Agpl.Gdk.Drawer_Figures;
