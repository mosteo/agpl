
--  Proxy for drawing in a GdkDrawable, adding capabilities like autoscale,
--  zoom, axis flipping...

with Agpl.Gdk.Drawer_Action;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;

package Agpl.Gdk.Drawer_Rectangle is

   pragma Elaborate_Body;

   type Object is new Agpl.Gdk.Drawer_Action.Object with private;

   function Create (Gc             : Gdk_GC;
                    X1, Y1, X2, Y2 : Float;
                    Fill           : Boolean := True) return Object;
   --

   procedure Prepare  (This   : in out Object;
                       Transf : in     Float_Matrix; -- Any transformation.
                       Min_X,
                       Max_X,
                       Min_Y,
                       Max_Y  :    out Float);
   --  Pre-transform the figure and say enclosing box.

   procedure Perform (This   : in out Object;
                      Transf : in     Float_Matrix; -- Scaling transformation.
                      Draw   :        Gdk_Drawable);
   --  Do the real drawing.

private

   type Object is new Agpl.Gdk.Drawer_Action.Object with record
      P1, P2 : Float_Vector;
      Fill   : Boolean;
      Gc     : Gdk_GC;
   end record;

end Agpl.Gdk.Drawer_Rectangle;
