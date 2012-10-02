
with Glib;

package body Agpl.Gdk.Drawer_Rectangle is

   ------------
   -- Create --
   ------------

   function Create (Gc             : Gdk_GC;
                    X1, Y1, X2, Y2 : Float;
                    Fill           : Boolean := True) return Object is
   begin
      return (Gc => Gc,
              Fill => Fill,
              P1 => (X1, Y1, 1.0),
              P2 => (X2, Y2, 1.0));
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
      This.P1 := Transf * This.P1;
      This.P2 := Transf * This.P2;
      Min_X := Float'Min (This.P1 (1), This.P2 (1));
      Min_Y := Float'Min (This.P1 (2), This.P2 (2));
      Max_X := Float'Max (This.P1 (1), This.P2 (1));
      Max_Y := Float'Max (This.P1 (2), This.P2 (2));
   end Prepare;

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This   : in out Object;
      Transf : in     Float_Matrix;
      Draw   :        Gdk_Drawable)
   is
      T1 : constant Float_Vector := Transf * This.P1;
      T2 : constant Float_Vector := Transf * This.P2;
      use Glib;
   begin
      Draw_Rectangle (Draw, This.Gc, This.Fill,
                      Gint (Float'Min (T1 (1), T2 (1))),
                      Gint (Float'Min (T1 (2), T2 (2))),
                      abs (Gint (T2 (1) - T1 (1))),
                      abs (Gint (T2 (2) - T1 (2))));
   end Perform;

end Agpl.Gdk.Drawer_Rectangle;
