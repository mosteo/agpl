with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Agpl.Cv;
with Agpl.If_Function;
with Agpl.Interfaces.C.Types;
with Agpl.Strings;
--  with Agpl.Trace; use Agpl.Trace;
with Agpl.Transf2D;

package body Agpl.Drawing.Figures is

   package Transform is new Transf2D (Float);

   function "+" (P : in Cv.Point2D) return Transform.Pose;
   pragma Inline ("+");
   function "+" (P : in Transform.Pose) return Cv.Point2D;
   pragma Inline ("+");

   function "+" (P : in Cv.Point2D) return Transform.Pose is
   begin
      return (P (P'First), P (P'First + 1), 0.0, 1.0);
   end "+";

   function "+" (P : in Transform.Pose) return Cv.Point2D is
      use Transform;
   begin
      return (P (X), P (Y), 1.0);
   end "+";

   -----------------
   -- Null_Figure --
   -----------------

   type Null_Type is new Drawable with null record;

   overriding
   procedure Draw (Fig : Null_Type; D : in out Drawer'Class) is null;

   function Null_Figure return Drawable'Class is
      function Null_Figure_X return Null_Type is
      begin
         return N : Null_Type do
            pragma Unreferenced (N);
            null;
         end return;
      end Null_Figure_X;
   begin
      return Null_Figure_X; -- To avoid bug in GPL 2008
   end Null_Figure;

   ----------
   -- Text --
   ----------

   type Text_Type (Length : Natural) is new Drawable with record
      X, Y : Float;
      Text : String (1 .. Length);
   end record;

   overriding
   procedure Draw (Fig : Text_Type; D : in out Drawer'Class) is
   begin
      D.Write (Fig.X, Fig.Y, Fig.Text);
   end Draw;

   function Text (X, Y : Float; S : String) return Drawable'Class is
      function Text_Internal return Text_Type is
      begin
         return Fig : Text_Type (S'Length) do
            Fig := (S'Length, X, Y, S);
         end return;
      end Text_Internal;
   begin
      return Text_Internal;
   end Text;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : Vprogress; Into : in out Drawer'Class) is
      use Agpl.Strings;

      function Iif is new If_Function (String);

      Engraving : constant := 0.08;
   begin
      Into.Set_Color ((0, 0, 0), 255);
      Draw_Rectangle (Into, This.X, This.Y,
                      This.X + This.Width, This.Y + This.Height);

      if This.Text_Above then
         Into.Write
           (This.X, This.Y + This.Height + 0.8,
            To_String (This.Val, 1) &
            Iif (This.Show_Pct,
              " (" & To_String (100.0 * This.Val / This.Max, 1) & "%)",
              ""));
      end if;

      Into.Set_Color ((213, 0, 82), 255);
      Fill_Rectangle (Into,
                      This.X + Engraving, This.Y + Engraving,
                      This.X + This.Width - Engraving,
                      This.Y + (This.Height * This.Val / This.Max) - Engraving);
   end Draw;

   ------------------------------
   -- Create_Bivariate_Ellipse --
   ------------------------------

   function Create_Bivariate_Ellipse
     (Mu     : Point;
      Th     : Float;
      Sigma  : Cov2d;
      Conf   : Confidence := 0.95;
      Curve  : Boolean    := True;
      Axes   : Boolean    := True;
      Points : Positive   := 20) return Bivariate_Ellipse
   is
   begin
      return
        (Mu     => Mu,
         Th     => Th,
         Sigma  => Sigma,
         Conf   => Conf,
         Curve  => Curve,
         Axes   => Axes,
         Points => Points);
   end Create_Bivariate_Ellipse;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : Bivariate_Ellipse; Into : in out Drawer'Class) is
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Real_Arrays;
      use Agpl.Interfaces;

      function Cdf_Chi_P_Inv (P : C.Types.Double; Nu : C.Types.Double) return C.Types.Double;
      pragma Import (C, Cdf_Chi_P_Inv, "gsl_cdf_chisq_Pinv");
      --  This requires both GSL and BLAS
      --  Nu: Degrees of freedom

      K : constant Float :=
            Float (Cdf_Chi_P_Inv (C.Types.Double (This.Conf), 2.0));

      Sigma : constant Real_Matrix := This.Sigma * K; -- Matrix * scalar

      subtype Vector2d is Point;
      subtype Matrix2d is Cov2d;

      Ddiag : Vector2d; -- Eigenvalues
      D     : Matrix2d := (others => (others => 0.0));
      Dsqrt : Matrix2d;
      V     : Matrix2d; -- Eigenvectors
   begin
      Eigensystem (Sigma, Ddiag, V);

      --  D should be a diagonal matrix, to match matlab.
      D (1, 1) := Ddiag (1);
      D (2, 2) := Ddiag (2);

      for R in D'Range (1) loop
         for C in D'Range (2) loop
            Dsqrt (R, C) := Sqrt (abs D (R, C));
         end loop;
      end loop;

      if This.Curve then
         declare
            U : Real_Matrix (1 .. 2, 1 .. This.Points);
            --  2 x Points
         begin
            for C in U'Range (2) loop
               U (1, C) := Cos (2.0 * Pi / Float (This.Points) * Float (C - 1));
               U (2, C) := Sin (2.0 * Pi / Float (This.Points) * Float (C - 1));
            end loop;

            declare
               W : Real_Matrix := (V * Dsqrt) * U;
               --  ((2x2)*(2x2))*(2xn) = (2xn)
               pragma Assert (W'Length (1) = Vector2d'Length);
               pragma Assert (W'Length (2) = This.Points);
            begin
               for C in W'Range (2) loop
                  --  Rotate:
                  declare
                     use Transform;
                     Rot : constant Transformation := Get_Rotation (This.Th);
                     P   : constant Pose := Rot * To_Pose (W (1, C), W (2, C));
                  begin
                     W (1, C) := P (X);
                     W (2, C) := P (Y);
                  end;
                  --  Move to Mu
                  W (1, C) := This.Mu (1) + W (1, C);
                  W (2, C) := This.Mu (2) + W (2, C);
               end loop;

               --  Draw ellipse
               for C1 in W'Range (2) loop
                  declare
                     C2 : Positive;
                  begin
                     if C1 = W'Last (2) then
                        C2 := W'First (2);
                     else
                        C2 := C1 + 1;
                     end if;

                     Into.Draw_Line (W (1, C1), W (2, C1),
                                     W (1, C2), W (2, C2));
                  end;
               end loop;
            end;
         end;
      end if;

      if This.Axes then
         declare
            use Transform;
            Rot : constant Transformation := Get_Rotation (This.Th);
            V1  : constant Pose := Rot * To_Pose (V (1, 1), V (2, 1));
            V2  : constant Pose := Rot * To_Pose (V (1, 2), V (2, 2));
            L : constant Vector2d := (Dsqrt (1, 1),
                                      Dsqrt (2, 2));
         begin
            Into.Draw_Line (This.Mu (1) - L (1) * V1 (X),
                            This.Mu (2) - L (1) * V1 (Y),
                            This.Mu (1) + L (1) * V1 (X),
                            This.Mu (2) + L (1) * V1 (Y));
            Into.Draw_Line (This.Mu (1) - L (2) * V2 (X),
                            This.Mu (2) - L (2) * V2 (Y),
                            This.Mu (1) + L (2) * V2 (X),
                            This.Mu (2) + L (2) * V2 (Y));
--              Into.Draw_Line (This.Mu (1) - L (1) * V (1, 1),
--                              This.Mu (2) - L (1) * V (2, 1),
--                              This.Mu (1) + L (1) * V (1, 1),
--                              This.Mu (2) + L (1) * V (2, 1));
--              Into.Draw_Line (This.Mu (1) - L (2) * V (1, 2),
--                              This.Mu (2) - L (2) * V (2, 2),
--                              This.Mu (1) + L (2) * V (1, 2),
--                              This.Mu (2) + L (2) * V (2, 2));
         end;
      end if;
   end Draw;

   type Robot_Type is new Drawable with record
      X, Y, A, S : Float;
   end record;

   procedure Draw (Fig : Robot_Type; D : in out Drawer'Class) is
   --  Points for a robot looking along the X axis.
      H : constant Float := Fig.S / 2.0;
      P1 : Cv.Point2D := ( - H, H, 1.0);
      P2 : Cv.Point2D := ( - H, - H, 1.0);
      P3 : Cv.Point2D := ( H, H, 1.0);
      P4 : Cv.Point2D := ( H, - H, 1.0);
      P5 : Cv.Point2D := ( H, 0.0, 1.0);
   begin
      --  Transform the points to reflect the true pose:
      declare
         use Transform;
         T : constant Transformation :=
               Get_Translation ( -Fig.X, -Fig.Y) * Get_Rotation ( -Fig.A);
      begin
         P1 := + (T * ( + P1));
         P2 := + (T * ( + P2));
         P3 := + (T * ( + P3));
         P4 := + (T * ( + P4));
         P5 := + (T * ( + P5));
      end;

      --  Arrow
      D.Draw_Line (P1 (1), P1 (2), P5 (1), P5 (2));
      D.Draw_Line (P2 (1), P2 (2), P5 (1), P5 (2));

      --  Base
      D.Draw_Line (P1 (1), P1 (2), P2 (1), P2 (2));

      --  Box
      D.Draw_Line (P1 (1), P1 (2), P3 (1), P3 (2));
      D.Draw_Line (P2 (1), P2 (2), P4 (1), P4 (2));
      D.Draw_Line (P3 (1), P3 (2), P4 (1), P4 (2));
   end Draw;

   -----------
   -- Robot --
   -----------

   function Robot (X, Y, A : Float;
                   Size    : Float := 0.5) return Drawable'Class is
   begin
      return Robot_Type'(X => X, Y => Y, A => A, S => Size);
   end Robot;

   -------------
   -- Segment --
   -------------

   function Segment (X1, Y1, X2, Y2 : Float) return Buffer.Object is
      B : Agpl.Drawing.Buffer.object;
   begin
      B.Draw_Line (X1, Y1, X2, Y2);
      return B;
   end Segment;

   ------------
   -- Target --
   ------------

   function Target (X, Y : Float; Size : Float := 1.0) return Buffer.Object is
      B : Agpl.Drawing.Buffer.object;
      D : constant Float := Size / 2.0;
   begin
      B.Draw_Line (X - D, Y - D, X + D, Y + D);
      B.Draw_Line (X - D, Y + D, X + D, Y - D);
      return B;
   end Target;

   --------------------
   -- Simple_History --
   --------------------

   function Simple_History (Values   : Float_Array)
                            return     Drawing.Buffer.Object
   is
      B : Agpl.Drawing.Buffer.Object;
      X : Float := 0.0;
   begin
      for I in Values'First .. Values'Last - 1 loop
         B.Draw_Line (X, Values (I), X + 1.0, Values (I + 1));
         X := X + 1.0;
      end loop;

      return B;
   end Simple_History;

   --------------------
   -- Simple_History --
   --------------------

   function Simple_History (Values   : Float_List)
                            return     Drawing.Buffer.Object
   is
      use Float_Lists;

      B : Agpl.Drawing.Buffer.object;
      X : Float := 0.0;

      procedure Draw (I : Cursor) is
      begin
         if Has_Element (Next (I)) then
            B.Draw_Line (X, Element (I), X + 1.0, Element (Next (I)));
            X := X + 1.0;
         end if;
      end Draw;

   begin
      Values.Iterate (Draw'Access);
      return B;
   end Simple_History;

   --------------------
   -- Simple_History --
   --------------------

   function Simple_History (Min, Max : Float;
                            Values   : Float_List;
                            Max_Vals : Natural;
                            Val_RGB  : Types.Rgb_Triplet := (0, 0, 255);
                            Axis_RGB : Types.Rgb_triplet := (0, 0, 0))
                            return     Drawing.Buffer.Object
   is
      B : Agpl.Drawing.Buffer.Object;
   begin
      B.Set_Color (Axis_RGB, 0);
      B.Draw_Line (0.0, Min, Float (Max_Vals), Min);
      B.Draw_Line (0.0, Min, 0.0, Max);

      B.Set_Color (Val_RGB, 0);
      Simple_History (Values).Draw (B);
      return B;
   end Simple_History;

   ----------------
   -- Add_Sample --
   ----------------

   procedure Add_Sample (List : in out Float_List; Sample : Float) is
   begin
      List.Append (Sample);
   end Add_Sample;

   ----------
   -- Trim --
   ----------

   procedure Trim (List : in out Float_List; Samples : Natural) is
   begin
      while Natural (List.Length) > Samples loop
         List.Delete_First;
      end loop;
   end Trim;

end Agpl.Drawing.Figures;
