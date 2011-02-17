package body Agpl.Drawing.Transformations is

   ---------------
   -- Get_Ratio --
   ---------------

   function Get_Ratio (Xorig1, Xorig2, Xdest1, Xdest2 : Float) return Float is
   begin
      if Xorig1 = Xorig2 then
         return 1.0;
      else
         return (Xdest2 - Xdest1) / (Xorig2 - Xorig1);
      end if;
   end Get_Ratio;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Transformer) is
   begin
      This.Mode := Filtering;
      This.Store.Clear;

      This.Xmin := Float'Last;
      This.Ymin := Float'Last;

      This.Xmax := Float'First;
      This.Ymax := Float'First;

      This.T    := Transf.Identity;
   end Clear;

   ---------
   -- Fit --
   ---------

   procedure Fit
     (This     : in out Transformer;
      Into     : in out Drawer'Class;
      X_Left,
      X_Right,
      Y_Bottom,
      Y_Top    :        Float;
      Square   :        Boolean := True)
   is
      Scale_X : Float := Get_Ratio (This.Xmin, This.Xmax, X_Left, X_Right);
      Scale_Y : Float := Get_Ratio (This.Ymin, This.Ymax, Y_Bottom, Y_Top);
   begin
      --  Compute transformation
      if Square then
         Scale_X := Float'Min (Scale_X, Scale_Y);
         Scale_Y := Scale_X;
      end if;

      --  We apply Matrix * Vector (i.e. right-to-left) so:
      This.T :=
        --  Final moving into the fit rectangle
        Get_Translation (-(X_Left + X_Right) / 2.0,
                         -(Y_Bottom + Y_Top) / 2.0) *
        --  Scaling centered in origin
        Get_Scaling (Scale_X, Scale_Y) *
        --  Initial moving to place on origin
        Get_Translation ((This.Xmin + This.Xmax) / 2.0,
                         (This.Ymin + This.Ymax) / 2.0);

      --  Set mode to drawing and re-do:
      This.Back := Into'Unchecked_Access;
      This.Mode := Drawing;
      This.Store.Draw (This);
      This.Mode := Filtering;
      This.Back := null;
   end Fit;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (This   : in out Transformer;
      X1, Y1,
      X2, Y2 : Float)
   is
   begin
      case This.Mode is
         when Filtering =>
            This.Get_Ranges (X1, Y1);
            This.Get_Ranges (X2, Y2);
            This.Store.Draw_Line (X1, Y1, X2, Y2);
         when Drawing =>
            declare
               V1 : constant Pose := This.T * To_Pose (X1, Y1);
               V2 : constant Pose := This.T * To_Pose (X2, Y2);
            begin
               This.Back.Draw_Line (V1 (X), V1 (Y), V2 (X), V2 (Y));
            end;
      end case;
   end Draw_Line;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (This   : in out Transformer;
      X1, Y1,
      X2, Y2 : Float)
   is
   begin
      case This.Mode is
         when Filtering =>
            This.Get_Ranges (X1, Y1);
            This.Get_Ranges (X2, Y2);
            This.Store.Fill_Rectangle (X1, Y1, X2, Y2);
         when Drawing =>
            declare
               V1 : constant Pose := This.T * To_Pose (X1, Y1);
               V2 : constant Pose := This.T * To_Pose (X2, Y2);
            begin
               This.Back.Fill_Rectangle (V1 (X), V1 (Y), V2 (X), V2 (Y));
            end;
      end case;
   end Fill_Rectangle;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (This  : in out Transformer;
      Rgb   :        Types.Rgb_Triplet;
      Alpha :        Types.Unsigned_8)
   is
   begin
      case This.Mode is
         when Filtering =>
            This.Store.Set_Color (Rgb, Alpha);
         when Drawing =>
            This.Back.Set_Color (Rgb, Alpha);
      end case;
   end Set_Color;

   -----------
   -- Write --
   -----------

   procedure Write
     (This : in out Transformer;
      X, Y : Float;
      Utf8 : String)
   is
   begin
      case This.Mode is
         when Filtering =>
            This.Get_Ranges (X, Y);
            This.Store.Write (X, Y, Utf8);
         when Drawing =>
            declare
               V : constant Pose := This.T * To_Pose (X, Y);
            begin
               This.Back.Write (V (Transf.X), V (Transf.Y), Utf8);
            end;
      end case;
   end Write;

   ----------------
   -- Get_Ranges --
   ----------------

   procedure Get_Ranges (This : in out Transformer; X, Y : Float) is
   begin
      This.Xmin := Float'Min (This.Xmin, X);
      This.Xmax := Float'Max (This.Xmax, X);

      This.Ymin := Float'Min (This.Ymin, Y);
      This.Ymax := Float'Max (This.Ymax, Y);
   end Get_Ranges;

end Agpl.Drawing.Transformations;
