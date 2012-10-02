
with Agpl.Cv;
with Agpl.Gdk.Drawer_Point;
with Agpl.Gdk.Drawer_Segment;
with Agpl.Transf2D;

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Agpl.Gdk.Drawer_Figures is

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

   ----------------
   -- Draw_Arrow --
   ----------------

   procedure Draw_Arrow (X1, Y1, X2, Y2 : in     Float;
                         Size           : in     Float;
                         Gc             : in     Gdk_GC;
                         Draw           : in out Drawer.Object)
   is
   begin
      Draw_Segment (X1, Y1, X2, Y2, Gc, Draw);

      --  Arrow
      declare
         H  : constant Float := Size;
         P1 : Cv.Point2D := (-H,   H, 1.0);
         P2 : Cv.Point2D := (-H, -H, 1.0);
         P5 : Cv.Point2D := (0.0, 0.0, 1.0);
         A  : constant Float := Arctan (Y2 - Y1, X2 - X1);
      begin
         --  Transform the points to reflect the true pose:
         declare
            use Transform;
            T : constant Transformation :=
                  Get_Translation (-X2, -Y2) * Get_Rotation (-A);
         begin
            P1 := +(T * (+P1));
            P2 := +(T * (+P2));
            P5 := +(T * (+P5));
         end;

         Draw_Segment (P1 (1), P1 (2), P5 (1), P5 (2), Gc, Draw);
         Draw_Segment (P2 (1), P2 (2), P5 (1), P5 (2), Gc, Draw);
      end;
   end Draw_Arrow;

   ----------------------
   -- Draw_Grid_Points --
   ----------------------

   procedure Draw_Grid_Points
     (X1, Y1, X2, Y2 : in     Float;
      Gc             : in     Gdk_GC;
      Draw           : in out Drawer.Object;
      Every_X        : in     Float := 1.0;
      Every_Y        : in     Float := 1.0)
   is
      Xmin : constant Float := Float'Min (X1, X2);
      Xmax : constant Float := Float'Max (X1, X2);
      Ymin : constant Float := Float'Min (Y1, Y2);
      Ymax : constant Float := Float'Max (Y1, Y2);

      C, R : Float;
   begin
      C := 0.0;
      while C <= Xmax loop
         R := 0.0;
         while R <= Ymax loop
            if C >= Xmin and then R >= Ymin then
               Draw.Draw (Drawer_Point.Create (Gc, C, R));
            end if;
            R := R + Every_Y;
         end loop;
         C := C + Every_X;
      end loop;

      C := 0.0;
      while C >= Xmin loop
         R := 0.0;
         while R >= Ymin loop
            if C <= Xmax and then R <= Ymax then
               Draw.Draw (Drawer_Point.Create (Gc, C, R));
            end if;
            R := R - Every_Y;
         end loop;
         C := C - Every_X;
      end loop;

      C := 0.0;
      while C <= Xmax loop
         R := 0.0;
         while R >= Ymin loop
            if C >= Xmin and then R <= Ymax then
               Draw.Draw (Drawer_Point.Create (Gc, C, R));
            end if;
            R := R - Every_Y;
         end loop;
         C := C + Every_X;
      end loop;

      C := 0.0;
      while C >= Xmin loop
         R := 0.0;
         while R <= Ymax loop
            if C <= Xmax and then R >= Ymin then
               Draw.Draw (Drawer_Point.Create (Gc, C, R));
            end if;
            R := R + Every_Y;
         end loop;
         C := C - Every_X;
      end loop;
   end Draw_Grid_Points;

   ---------------
   -- Draw_Plus --
   ---------------

   procedure Draw_Plus
     (X, Y : in     Float;
      Gc   : in     Gdk_GC;
      Draw : in out Drawer.Object;
      Size : in     Float := 1.0)
   is
      H    : constant Float := Size / 2.0;
   begin
      Draw.Draw (Drawer_Segment.Create (Gc, X,     Y - H, X,     Y + H));
      Draw.Draw (Drawer_Segment.Create (Gc, X - H, Y,     X + H, Y));
   end Draw_Plus;

   ----------------
   -- Draw_Robot --
   ----------------

   procedure Draw_Robot (X, Y, A : in     Float;
                         Gc      : in     Gdk_GC;
                         Draw    : in out Drawer.Object;
                         Size    : in     Float := 0.5)
   is
      --  Points for a robot looking along the X axis.
      H : constant Float := Size / 2.0;
      P1 : Cv.Point2D := (-H, H, 1.0);
      P2 : Cv.Point2D := (-H, -H, 1.0);
      P3 : Cv.Point2D := (H, H, 1.0);
      P4 : Cv.Point2D := (H, -H, 1.0);
      P5 : Cv.Point2D := (H, 0.0, 1.0);
   begin
      --  Transform the points to reflect the true pose:
      declare
         use Transform;
         T : constant Transformation :=
               Get_Translation (-X, -Y) * Get_Rotation (-A);
      begin
         P1 := +(T * (+P1));
         P2 := +(T * (+P2));
         P3 := +(T * (+P3));
         P4 := +(T * (+P4));
         P5 := +(T * (+P5));
      end;

      --  Arrow
      Draw_Segment (P1 (1), P1 (2), P5 (1), P5 (2), Gc, Draw);
      Draw_Segment (P2 (1), P2 (2), P5 (1), P5 (2), Gc, Draw);

      --  Base
      Draw_Segment (P1 (1), P1 (2), P2 (1), P2 (2), Gc, Draw);

      --  Box
      Draw_Segment (P1 (1), P1 (2), P3 (1), P3 (2), Gc, Draw);
      Draw_Segment (P2 (1), P2 (2), P4 (1), P4 (2), Gc, Draw);
      Draw_Segment (P3 (1), P3 (2), P4 (1), P4 (2), Gc, Draw);
   end Draw_Robot;

   ------------------
   -- Draw_Segment --
   ------------------

   procedure Draw_Segment (X1, Y1, X2, Y2 : in     Float;
                           Gc             : in     Gdk_GC;
                           Draw           : in out Drawer.Object)
   is
   begin
      Draw.Draw (Drawer_Segment.Create (Gc, X1, Y1, X2, Y2));
   end Draw_Segment;

   -----------------
   -- Draw_Vector --
   -----------------

   procedure Draw_Vector (X, Y : in Float;
                          A, L : in Float;
                          Gc   : in Gdk_GC;
                          Draw : in out Drawer.Object)
   is
      X2 : constant Float := X + L * Cos (A);
      Y2 : constant Float := Y + L * Sin (A);
   begin
      Draw_Segment (X, Y, X2, Y2, Gc, Draw);
   end Draw_Vector;

end Agpl.Gdk.Drawer_Figures;
