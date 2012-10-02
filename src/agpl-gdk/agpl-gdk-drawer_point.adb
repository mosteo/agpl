
with Glib;

package body Agpl.Gdk.Drawer_Point is

   ------------
   -- Create --
   ------------

   function Create (Gc : Gdk_GC; X, Y : Float) return Object is
   begin
      return (Gc => Gc, P => (X, Y, 1.0));
   end Create;

   -------------
   -- Prepare --
   -------------

   procedure Prepare
     (This   : in out Object;
      Transf : in     Float_Matrix;
      Min_X,
      Max_X,
      Min_Y,
      Max_Y  :    out Float)
   is
   begin
      This.P := H (Transf * This.P);
      Min_X := This.P (1);
      Min_Y := This.P (2);
      Max_X := Min_X;
      Max_Y := Min_Y;
   end Prepare;

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This   : in out Object;
      Transf : in     Float_Matrix;
      Draw   :        Gdk_Drawable)
   is
      use Glib;
      P : constant Float_Vector := H (Transf * This.P);
   begin
      Draw_Point (Draw, This.Gc, Gint (P (1)), Gint (P (2)));
   end Perform;

end Agpl.Gdk.Drawer_Point;
