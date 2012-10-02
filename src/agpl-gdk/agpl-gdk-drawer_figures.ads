
--  Some stock figures to draw easily.

with Agpl.Gdk.Drawer;

with Gdk.GC; use Gdk.GC;

package Agpl.Gdk.Drawer_Figures is

   pragma Elaborate_Body;

   procedure Draw_Arrow (X1, Y1, X2, Y2 : in     Float;
                         Size           : in     Float;
                         Gc             : in     Gdk_GC;
                         Draw           : in out Drawer.Object);
   --  Draw a segment with arrow head at X2, Y2 of @Size@ size.

   procedure Draw_Grid_Points
     (X1, Y1, X2, Y2 : in     Float;
      Gc             : in     Gdk_GC;
      Draw           : in out Drawer.Object;
      Every_X        : in     Float := 1.0;
      Every_Y        : in     Float := 1.0);
   --  Draw a grid made of points (origin at 0, 0).

   procedure Draw_Plus (X, Y : in     Float;
                        Gc   : in     Gdk_GC;
                        Draw : in out Drawer.Object;
                        Size : in     Float := 1.0);
   --  A cross sign.

   procedure Draw_Robot (X, Y, A : in     Float;
                         Gc      : in     Gdk_GC;
                         Draw    : in out Drawer.Object;
                         Size    : in     Float := 0.5);
   --  A typical rectangular-with-arrow shaped robot.

   procedure Draw_Segment (X1, Y1, X2, Y2 : in     Float;
                           Gc             : in     Gdk_GC;
                           Draw           : in out Drawer.Object);
   --  Drawing of a segment in just a call.

   procedure Draw_Vector (X, Y : in Float;
                          A, L : in Float;
                          Gc   : in Gdk_GC;
                          Draw : in out Drawer.Object);
   --  Draw a oriented line starting at point XY, with angle A and length L.

end Agpl.Gdk.Drawer_Figures;
