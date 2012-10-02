with Agpl.Gdk.Pango_Layout;
with Agpl.Trace; use Agpl.Trace;
with Glib;

package body Agpl.Gdk.Drawer_Write is

   ------------
   -- Create --
   ------------

   function Create (Gc   : Gdk_GC;
                    W    : Gtk_Widget;
                    X, Y : Float;
                    Utf8 : String) return Object is
   begin
      return (Gc   => Gc,
              W    => W,
              P    => (X, Y, 1.0),
              Last => Utf8'Length,
              Utf8 => Utf8);
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
      This.P := Transf * This.P;
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
      T      : constant Float_Vector := Transf * This.P;
      Layout :          Agpl.Gdk.Pango_Layout.Object
        (Create_Pango_Layout (This.W, This.Utf8));
   begin
      Draw_Layout (Draw, This.Gc,
                   Gint (T (1)), Gint (T (2)),
                   Layout.Layout);
   exception
      when others =>
         Log ("OBERFLOW: " &
              This.P (1)'Img & This.P (2)'Img &
              T (1)'Img & T (2)'Img, Error);
   end Perform;

end Agpl.Gdk.Drawer_Write;
