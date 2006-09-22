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

with Glib;

package body Agpl.Gdk.Drawer is

   subtype Action is Drawer_Action.Object;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Object; X : Drawer_Action.Object'Class) is
   begin
      This.Queue.Append (X);
   end Draw;

   ----------------
   -- Draw_Begin --
   ----------------

   procedure Draw_Begin (This : in out Object) is
      pragma Unreferenced (This);
   begin
      null;
   end Draw_Begin;

   --------------
   -- Draw_End --
   --------------

   procedure Draw_End (This : in out Object) is
      use Action_Lists;
      use Drawer_Action;

      ----------------
      -- Get_Ranges --
      ----------------

      procedure Get_Ranges (A : in out Action'Class) is
         Min_X, Max_X, Min_Y, Max_Y : Float;
      begin
         Prepare (A, This.Pretransf,
                  Min_X => Min_X, Max_X => Max_X,
                  Min_Y => Min_Y, Max_Y => Max_Y);

         if This.Zoom_X = Automatic then
            This.Min_X := Float'Min (This.Min_X, Min_X);
            This.Max_X := Float'Max (This.Max_X, Max_X);
         end if;
         if This.Zoom_Y = Automatic then
            This.Min_Y := Float'Min (This.Min_Y, Min_Y);
            This.Max_Y := Float'Max (This.Max_Y, Max_Y);
         end if;
      end Get_Ranges;

      procedure Get_Ranges (I : Cursor) is
      begin
         Update_Element (This.Queue, I, Get_Ranges'Access);
      end Get_Ranges;

      ----------
      -- Draw --
      ----------

      procedure Draw (A : in out Action'Class) is
      begin
         Perform (A, This.Transform, This.Draw);
      end Draw;

      procedure Draw (I : Cursor) is
      begin
         Update_Element (This.Queue, I, Draw'Access);
      end Draw;

      W_Width,
      W_Height : Float; -- Sizes for the canvas
      Z_Width,
      Z_Height : Float; -- Sizes for the autoscale
      Offset_X,
      Offset_Y : Float := 0.0; -- To be used when aspect ratio is enforced.
      Ratio_X,
      Ratio_Y  : Float := 1.0;
      --  Ratio_Margin_X,
      --  Ratio_Margin_Y : Float := 0.0;
   begin
      if This.Zoom_X = Automatic then
         This.Max_X := Float'First;
         This.Min_X := Float'Last;
      end if;

      if This.Zoom_Y = Automatic then
         This.Max_Y := Float'First;
         This.Min_Y := Float'Last;
      end if;

      --  Get scales
      Iterate (This.Queue, Get_Ranges'Access);

      Get_Size (This.Draw,
                Width  => Glib.Gint (W_Width),
                Height => Glib.Gint (W_Height));
      Z_Width  := abs (This.Max_X - This.Min_X) * This.Margin;
      Z_Height := abs (This.Max_Y - This.Min_Y) * This.Margin;

      Ratio_X := W_Width  / Z_Width;
      Ratio_Y := W_Height / Z_Height;

      --  We need the adjusted margins here:
      if This.Keep_Ratio then
         if Ratio_X > Ratio_Y then
            Offset_X := abs (W_Width - Z_Width * Ratio_Y) / 2.0;
            Ratio_X  := Ratio_Y;
         else
            Offset_Y := abs (W_Height - Z_Height * Ratio_X) / 2.0;
            Ratio_Y  := Ratio_X;
         end if;
      end if;

      --  Add margins
      Offset_X := Offset_X + W_Width * (This.Margin - 1.0) / 2.0 ;
      Offset_Y := Offset_Y + W_Height * (This.Margin - 1.0) / 2.0;

      --  Here is injected the correction cartesian/bottom-up
      This.Transform :=
        ((Ratio_X, 0.0,
          -This.Min_X * Ratio_X + Offset_X),
         (0.0, -Ratio_Y,
          W_Height - (- This.Min_Y * Ratio_Y + Offset_Y)),
         (0.0, 0.0, 1.0));

--      This.Transformb := Inverse (This.Transform);

      --  Actual drawing
      Iterate (This.Queue, Draw'Access);

      --  Clear
      Clear (This.Queue);
   end Draw_End;

   ------------------
   -- Get_Drawable --
   ------------------

   function Get_Drawable (This : in Object) return Gdk_Drawable is
   begin
      return This.Draw;
   end Get_Drawable;

   ---------------
   -- Immediate --
   ---------------

   function Immediate (This : in Object) return Boolean is
   begin
      return This.Zoom_X /= Automatic and then This.Zoom_Y /= Automatic;
   end Immediate;

   -----------------------
   -- Keep_Aspect_Ratio --
   -----------------------

   procedure Keep_Aspect_Ratio (This : in out Object; Keep : Boolean := True) is
   begin
      This.Keep_Ratio := Keep;
   end Keep_Aspect_Ratio;

   ------------------
   -- Set_Drawable --
   ------------------

   procedure Set_Drawable (This : in out Object; D : Gdk_Drawable) is
   begin
      This.Draw := D;
   end Set_Drawable;

   ----------------
   -- Set_Flip_X --
   ----------------

   procedure Set_Flip_X (This : in out Object) is
   begin
      This.Transform (1, 1) := -This.Transform (1, 1);
   end Set_Flip_X;

   ----------------
   -- Set_Flip_Y --
   ----------------

   procedure Set_Flip_Y (This : in out Object) is
   begin
      This.Transform (2, 2) := -This.Transform (2, 2);
   end Set_Flip_Y;

   ----------------
   -- Set_Margin --
   ----------------

   procedure Set_Margin (This : in out Object; Margin : Float := 1.1) is
   begin
      This.Margin := Margin;
   end Set_Margin;

   -----------------
   -- Set_Range_X --
   -----------------

   procedure Set_Range_X (This : in out Object; Min, Max : Float) is
   begin
      This.Min_X  := Min;
      This.Max_X  := Max;
      This.Zoom_X := Explicit;
   end Set_Range_X;

   ----------------------
   -- Set_Range_X_Auto --
   ----------------------

   procedure Set_Range_X_Auto (This : in out Object) is
   begin
      This.Zoom_X := Automatic;
   end Set_Range_X_Auto;

   -----------------
   -- Set_Range_Y --
   -----------------

   procedure Set_Range_Y (This : in out Object; Min, Max : Float) is
   begin
      This.Min_Y  := Min;
      This.Max_Y  := Max;
      This.Zoom_Y := Explicit;
   end Set_Range_Y;

   ----------------------
   -- Set_Range_Y_Auto --
   ----------------------

   procedure Set_Range_Y_Auto (This : in out Object) is
   begin
      This.Zoom_Y := Automatic;
   end Set_Range_Y_Auto;

   --------------
   -- Set_Swap --
   --------------

   procedure Set_Swap (This : in out Object) is
      procedure Swap (A, B : in out Float) is
         Tmp : constant Float := B;
      begin
         B := A;
         A := Tmp;
      end Swap;
   begin
      Swap (This.Pretransf (1, 1), This.Pretransf (1, 2));
      Swap (This.Pretransf (2, 1), This.Pretransf (2, 2));
   end Set_Swap;

   ------------------------
   -- Set_Transformation --
   ------------------------

   procedure Set_Transformation
     (This           : in out Object;
      Transformation :        Float_Matrix)
   is
   begin
      This.Pretransf := Transformation;
   end Set_Transformation;

   ---------------
   -- Transform --
   ---------------

   function Transform (This  : in Object;
                       Point : in Float_Vector)
                       return     Float_Vector
   is
   begin
      return H (This.Transform * (This.Pretransf * Point));
   end Transform;

   --------------------
   -- Transform_Back --
   --------------------

   function Transform_Back (This  : in Object;
                            Point : in Float_Vector)
                            return     Float_Vector
   is
   begin
      return
        H (Inverse (This.Pretransf) *
             (Inverse (This.Transform) * Point));

      --  return This.Transformb * Point;
   end Transform_Back;

end Agpl.Gdk.Drawer;
