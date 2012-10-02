

--  Proxy for drawing in a GdkDrawable, adding capabilities like autoscale,
--  zoom, axis flipping...

with Agpl.Gdk.Drawer_Action;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;
with Gtk.Widget;   use Gtk.Widget;

package Agpl.Gdk.Drawer_Write is

   pragma Elaborate_Body;

   type Object (<>) is new Agpl.Gdk.Drawer_Action.Object with private;

   function Create (Gc   : Gdk_GC;
                    W    : Gtk_Widget;
                    X, Y : Float;
                    Utf8 : String) return Object;

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

   type Object (Last : Natural) is new Agpl.Gdk.Drawer_Action.Object with record
      Gc     : Gdk_GC;
      W      : Gtk_Widget;
      P      : Float_Vector;
      Utf8   : String (1 .. Last);
   end record;

end Agpl.Gdk.Drawer_Write;
