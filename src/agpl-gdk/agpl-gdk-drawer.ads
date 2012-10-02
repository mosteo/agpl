--  Proxy for drawing in a GdkDrawable, adding capabilities like autoscale,
--  zoom, axis flipping...

with Agpl.Drawing;
with Agpl.Gdk.Drawer_Action;
with Agpl.Gdk.Palette;
with Agpl.Types;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;
with Gtk.Widget;   use Gtk.Widget;

--  with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Agpl.Gdk.Drawer is

   pragma Elaborate_Body;

   Log_Section : constant String := "agpl.gdk.drawer";

   type Zoom_Kinds is (Explicit, Automatic);

   type Object is new Drawing.Drawer with private;
   --  The drawer itself.

   overriding
   procedure Draw_Line (This   : in out Object;
                        X1, Y1,
                        X2, Y2 : Float);

   overriding
   procedure Fill_Rectangle (This   : in out Object;
                             X1, Y1,
                             X2, Y2 : Float);

   overriding
   procedure Set_Color (This  : in out Object;
                        Rgb   :        Types.Rgb_Triplet;
                        Alpha :        Types.Unsigned_8);

   overriding
   procedure Write (This : in out Object;
                    X, Y : Float;
                    Utf8 : String);

   --  Here starts the old ways.

   procedure Draw (This : in out Object; X : Drawer_Action.Object'Class);

   procedure Draw (This : in out Object; X : Drawer_Action.Action_List);

   procedure Draw_Begin (This : in out Object);
   --  Reset internal zoom structures.

   procedure Draw_End (This : in out Object);
   --  All drawing commands are performed.

   procedure Draw_Copy (This :     Object;
                        List : out Drawer_Action.Action_List);
   --  Get a copy of the pending actions

   procedure Keep_Aspect_Ratio (This : in out Object; Keep : Boolean := True);
   --  Defaults to Yes.

   function Get_Drawable (This : in Object) return Gdk_Drawable;

   procedure Set_Drawable (This : in out Object; D : Gdk_Drawable);
   --  Where to draw into. Must be set before any actual drawing is attempted.

   procedure Set_Widget (This : in out Object; W : Gtk_Widget);
   --  Needed only for text operations

   procedure Set_Flip_X (This : in out Object);
   --  Coordinates in screen go left-right, top-down. Usually, Cartesian
   --  coordinates go bottom-up. These two can be changed with the Flip functions.
   --  Two consecutive flips cancel each other.

   procedure Set_Flip_Y (This : in out Object);
   --  By default, this flipping is on to have cartesian reference.

   procedure Set_Margin (This : in out Object; Margin : Float := 1.1);
   --  Ratio of margin around the zoom.

   procedure Set_Range_X (This : in out Object; Min, Max : Float);
   --  Forced zoom in X range.

   procedure Set_Range_X_Auto (This : in out Object);
   --  Auto zoom for X axis

   procedure Set_Range_Y (This : in out Object; Min, Max : Float);
   --  Forced zoom in Y range.

   procedure Set_Range_Y_Auto (This : in out Object);

   procedure Set_Swap (This : in out Object);
   --  X and Y coordinates will be swapped prior to all other transforms

   procedure Set_Transformation (This           : in out Object;
                                 Transformation :        Float_Matrix);
   --  Any transformation to the input coordinates.

   function Transform (This  : in Object;
                       Point : in Float_Vector)
                       return     Float_Vector;
   --  Apply current transformation to a given point
   --  I.E. go from world to screen coordinates
   --  You must apply this *after* Draw_End, since the
   --  transformation is computed there.

   function Transform_Back (This  : in Object;
                            Point : in Float_Vector)
                            return     Float_Vector;
   pragma Precondition (Point (Point'Last) = 1.0); -- Homogeneous transform.
   --  Apply inverse transformation.
   --  I.E. go from screen to world coordinates.
   --  This function is currently very expensive!!!!!
   --  (The inverse is computed every time)

private

   function Immediate (This : in Object) return Boolean;
   --  Says if actions can be immediately drawn.

   type Object is new Drawing.Drawer with record
      Keep_Ratio : Boolean      := True;
      Zoom_X     : Zoom_Kinds   := Automatic;
      Zoom_Y     : Zoom_Kinds   := Automatic;
      Margin     : Float        := 1.1;

      Pretransf  : Float_Matrix := Identity;
      --  This transformation is used for flipping and X <-> Y swapping.
      --  Is applied before the autoscaling.
      --  Also for rotation.

      Transform  : Float_Matrix := Identity;
      --  This transformation is used for scaling and traslation.

      --  Transformb : Float_Matrix := Identity;
      --  Inverse transformation

      Min_X,
      Max_X,
      Min_Y,
      Max_Y      : Float;

      Draw       : Gdk_Drawable;
      Widget     : Gtk_Widget; -- Needed for text operations

      Queue      : Drawer_Action.Action_List;

      Pal        : aliased Palette.Object;
      Curr_Gc    :         Gdk_GC;
   end record;

end Agpl.Gdk.Drawer;
