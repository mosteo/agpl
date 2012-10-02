
with Glib;

package body Agpl.Gdk.Drawer_Line is

   ------------
   -- Create --
   ------------

   function Create (Gc : Gdk_GC; X1, Y1, X2, Y2 : Float) return Object is
   begin
      return (Gc => Gc,
              P1 => (X1, Y1, 1.0),
              P2 => (X2, Y2, 1.0));
   end Create;

   function Create (Gc : Gdk_GC; A, B, C : Float) return Object is
      Line  : constant Float_Vector := (A, B, C);
      Ord0  : constant Float_Vector := (0.0, 0.0, 1.0) ** (0.0, 1.0, 1.0);
      OrdW  : constant Float_Vector := (1.0, 0.0, 1.0) **
                                       (1.0, 1.0, 1.0);
      Absc0 : constant Float_Vector := (0.0, 0.0, 1.0) ** (1.0, 0.0, 1.0);
      AbscH : constant Float_Vector := (0.0, 1.0, 1.0) **
                                       (1.0, 1.0, 1.0);
   begin
      if B = 0.0 then
         return (Gc => Gc,
                 P1 => (C, 0.0, 1.0),
                 P2 => (C, 1.0, 1.0));
      else
         declare
            Slope : constant Float := abs (-B / A);
         begin
            if Slope > 1.0 then
               --  Low slope, better crossings with ordinates axis.
               declare
                  Y0 : constant Float_Vector := H (Ord0 ** Line);
                  YW : constant Float_Vector := H (OrdW ** Line);
               begin
                  return (Gc => Gc,
                          P1 => (Y0 (1), Y0 (2), 1.0),
                          P2 => (YW (1), YW (2), 1.0));
               end;
            else
               declare
                  X0 : constant Float_Vector := H (Absc0 ** Line);
                  XH : constant Float_Vector := H (AbscH ** Line);
               begin
                  return (Gc => Gc,
                          P1 => (X0 (1), X0 (2), 1.0),
                          P2 => (XH (1), XH (2), 1.0));
               end;
            end if;
         end;
         end if;
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
      Width, Height : Gint;
   begin
      Get_Size (Draw, Width => Width, Height => Height);

      --  Special cases:
      if T1 (1) = T2 (1) then
         Draw_Line (Draw, This.Gc,
                    0,      Gint (T1 (2)),
                    Height, Gint (T2 (2)));
      elsif T1 (2) = T2 (2) then
         Draw_Line (Draw, This.Gc,
                    Gint (T1 (1)), 0,
                    Gint (T2 (1)), Width);
      else
         --  Find crossings with begin and end:
         declare
            Ord0  : constant Float_Vector := (0.0, 0.0, 1.0) ** (0.0, 1.0, 1.0);
            OrdW  : constant Float_Vector := (Float (Width), 0.0, 1.0) **
              (Float (Width), 1.0, 1.0);
            Absc0 : constant Float_Vector := (0.0, 0.0, 1.0) ** (1.0, 0.0, 1.0);
            AbscH : constant Float_Vector := (0.0, Float (Height), 1.0) **
              (1.0, Float (Height), 1.0);
            Line  : constant Float_Vector := T1 ** T2;
            Slope : constant Float :=
                      abs ((T1 (2) - T2 (2)) / (T1 (1) - T2 (1)));
         begin
            if Slope < 1.0 then
               --  Low slope, better crossings with ordinates axis.
               declare
                  Y0 : constant Float_Vector := H (Ord0 ** Line);
                  YW : constant Float_Vector := H (OrdW ** Line);
               begin
                  Draw_Line (Draw, This.Gc,
                    Gint (Y0 (1)), Gint (Y0 (2)),
                    Gint (YW (1)), Gint (YW (2)));
               end;
            else
               declare
                  X0 : constant Float_Vector := H (Absc0 ** Line);
                  XH : constant Float_Vector := H (AbscH ** Line);
               begin
                  Draw_Line (Draw, This.Gc,
                    Gint (X0 (1)), Gint (X0 (2)),
                    Gint (XH (1)), Gint (XH (2)));
               end;
            end if;
         end;
      end if;
   end Perform;

end Agpl.Gdk.Drawer_Line;
