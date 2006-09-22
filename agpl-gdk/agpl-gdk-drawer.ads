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

--  Proxy for drawing in a GdkDrawable, adding capabilities like autoscale,
--  zoom, axis flipping...

with Agpl.Gdk.Drawer_Action;

with Gdk.Drawable; use Gdk.Drawable;
--  with Gtk.Widget;   use Gtk.Widget;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Agpl.Gdk.Drawer is

   pragma Elaborate_Body;

   type Zoom_Kinds is (Explicit, Automatic);

   type Object is tagged private;
   --  The drawer itself.

   procedure Draw (This : in out Object; X : Drawer_Action.Object'Class);

   procedure Draw_Begin (This : in out Object);
   --  Reset internal zoom structures.

   procedure Draw_End (This : in out Object);
   --  All drawing commands are performed.

   procedure Keep_Aspect_Ratio (This : in out Object; Keep : Boolean := True);
   --  Defaults to Yes.

   function Get_Drawable (This : in Object) return Gdk_Drawable;

   procedure Set_Drawable (This : in out Object; D : Gdk_Drawable);
   --  Where to draw into. Must be set before any actual drawing is attempted.

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
   --  Apply inverse transformation.
   --  I.E. go from screen to world coordinates.
   --  This function is currently very expensive!!!!!
   --  (The inverse is computed every time)

private

   function Immediate (This : in Object) return Boolean;
   --  Says if actions can be immediately drawn.

   package Action_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Drawer_Action.Object'Class, Drawer_Action."=");

   type Object is tagged record
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

      Queue      : Action_Lists.List;
   end record;

end Agpl.Gdk.Drawer;
