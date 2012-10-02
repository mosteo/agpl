with Agpl.Gdk.Drawer_Rectangle;
with Agpl.Gdk.Drawer_Segment;
with Agpl.Gdk.Drawer_Write;
with Agpl.Gdk.Managed;
with Agpl.Trace; use Agpl.Trace;
with Glib;

package body Agpl.Gdk.Drawer is

   subtype Action is Drawer_Action.Object;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (This   : in out Object;
                        X1, Y1,
                        X2, Y2 : Float)
   is
      Line : constant Drawer_Segment.Object :=
               Drawer_Segment.Create (This.Curr_Gc, X1, Y1, X2, Y2);
   begin
      Draw (This, Line);
   end Draw_Line;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle (This   : in out Object;
                             X1, Y1,
                             X2, Y2 : Float)
   is
      Fill : constant Drawer_Rectangle.Object :=
               Drawer_Rectangle.Create (This.Curr_Gc, X1, Y1, X2, Y2,
                                        Fill => True);
   begin
      Draw (This, Fill);
   end Fill_Rectangle;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (This  : in out Object;
                        Rgb   :        Types.Rgb_triplet;
                        Alpha :        Types.Unsigned_8)
   is
      pragma Unreferenced (Alpha);
      use Palette;
   begin
      This.Curr_Gc :=
        Get_Color (This.Pal,
                   Get_Color_Name (Rgb.R, Rgb.G, Rgb.B));
   end Set_Color;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Object;
                    X, Y : Float;
                    Utf8 : String)
   is
      Text : constant Drawer_Write.Object :=
               Drawer_Write.Create (This.Curr_Gc, This.Widget, X, Y, Utf8);
   begin
      Draw (This, Text);
   end Write;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Object; X : Drawer_Action.Object'Class) is
   begin
      This.Queue.Append (X);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Object; X : Drawer_Action.Action_List) is
      use Drawer_Action.Action_Lists;
      procedure Draw (I : Cursor) is
      begin
         Draw (This, Element (I));
      end Draw;
   begin
      X.Iterate (Draw'Access);
   end Draw;

   ----------------
   -- Draw_Begin --
   ----------------

   procedure Draw_Begin (This : in out Object) is
      pragma Unreferenced (This);
   begin
      null;
   end Draw_Begin;

   ---------------
   -- Draw_Copy --
   ---------------

   procedure Draw_Copy (This :     Object;
                        List : out Drawer_Action.Action_List) is
   begin
      List := This.Queue;
   end Draw_Copy;

   --------------
   -- Draw_End --
   --------------

   procedure Draw_End (This : in out Object) is
      use Drawer_Action.Action_Lists;
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
         pragma Precondition (Managed.In_Gtk_Thread);
      begin
         pragma Warning ("Should this be done in the GTK thread??");
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
      if This.Queue.Is_Empty then
         return; -- Nothing to draw
      end if;

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

      --  div 0 protection
      if Z_Width < 0.0000001 then
         Z_Width := 1.0;
      end if;
      if Z_Height < 0.0000001 then
         Z_Height := 1.0;
      end if;

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
      Offset_X := Offset_X + W_Width * (This.Margin - 1.0) / 2.0;
      Offset_Y := Offset_Y + W_Height * (This.Margin - 1.0) / 2.0;

      --  Here is injected the correction cartesian/bottom-up
      This.Transform :=
        ((Ratio_X, 0.0,
          -This.Min_X * Ratio_X + Offset_X),
         (0.0, -Ratio_Y,
          W_Height - (-This.Min_Y * Ratio_Y + Offset_Y)),
         (0.0, 0.0, 1.0));

      --      This.Transformb := Inverse (This.Transform);
      --      Not working?

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
      This.Draw    := D;
      This.Pal     := Palette.Create (D);
      This.Curr_Gc := Palette.Get_Color (This.Pal, "black");
   end Set_Drawable;

   ----------------
   -- Set_Widget --
   ----------------

   procedure Set_Widget (This : in out Object; W : Gtk_Widget) is
   begin
      This.Widget := W;
   end Set_Widget;

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
   exception
      when others =>
         Log ("Transform: Couldn't transform", Error, Log_Section);
         return Point;
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
   exception
      when others =>
         Log ("Transform_Back: Couldn't transform", Error, Log_Section);
         Log ("Pretransf:", Error, Log_Section);
         Print (This.Pretransf);
         Log ("Transform:", Error, Log_Section);
         Print (This.Transform);
         return Point;
   end Transform_Back;

end Agpl.Gdk.Drawer;
