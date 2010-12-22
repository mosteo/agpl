 

--  Drawing points

with Agpl.Gdk.Drawer_Action;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;

package Agpl.Gdk.Drawer_Point is

   pragma Elaborate_Body;

   type Object is new Agpl.Gdk.Drawer_Action.Object with private;

   function Create (Gc : Gdk_Gc; X, Y : Float) return Object;

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
      P      : Float_Vector;
      Gc     : Gdk_Gc;
   end record;

end Agpl.Gdk.Drawer_Point;
