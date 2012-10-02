
with Glib;

with Ada.Numerics; use Ada.Numerics;

package body Agpl.Gdk.Drawer_Arc is

   ------------
   -- Create --
   ------------

   function Create
     (Gc     : Gdk_GC;
      X, Y   : Float;
      Width  : Float;
      Height : Float;
      Start  : Float;
      Finish : Float;
      Fill   : Boolean := False)
      return Object
   is
   begin
      return (Gc   => Gc,
              P    => (X, Y, 1.0),
              Size => (Width, Height, 1.0),
              Ini  => Start,
              Fin  => Finish,
              Fill => Fill);
   end Create;

   -------------------
   -- Create_Circle --
   -------------------

   function Create_Circle (Gc      : Gdk_GC;
                           X, Y, R : Float;
                           Fill    : Boolean := False) return Object is
   begin
      return (Gc   => Gc,
              P    => (X, Y, 1.0),
              Size => (R * 2.0, R * 2.0, 1.0),
              Ini  => 0.0,
              Fin  => Pi * 2.0,
              Fill => Fill);
   end Create_Circle;

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
      This.P    := H (Transf * This.P);
      This.Size := H (Transf * This.Size);
      This.Size := (abs (This.Size (1)), abs (This.Size (2)), This.Size (3));
      Min_X     := This.P (1) - This.Size (1) / 2.0;
      Max_X     := This.P (1) + This.Size (1) / 2.0;
      Min_Y     := This.P (2) - This.Size (2) / 2.0;
      Max_Y     := This.P (2) + This.Size (2) / 2.0;
   end Prepare;

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This   : in out Object;
      Transf : in     Float_Matrix;
      Draw   :        Gdk_Drawable)
   is
      function To_64grad (Radians : in Float) return Float;
      pragma Inline (To_64grad);
      function To_64grad (Radians : in Float) return Float is
      begin
         return Radians / Pi * 180.0 * 64.0;
      end To_64grad;

      PT : constant Float_Vector := H (Transf * This.P);
      ST : constant Float_Vector :=
             (abs (This.Size (1) * Transf (1, 1)),
              abs (This.Size (2) * Transf (2, 2)), 1.0); -- Just the scaling.

      use Glib;
   begin
      Draw_Arc (Draw, This.Gc,
                This.Fill,
                Gint (PT (1) - ST (1) / 2.0), Gint (PT (2) - ST (2) / 2.0),
                Gint (ST (1)), Gint (ST (2)),
                Gint (To_64grad (This.Ini)),
                Gint (To_64grad (This.Fin)));
   end Perform;

end Agpl.Gdk.Drawer_Arc;
