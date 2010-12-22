with Agpl.Constants;

procedure Agpl.Drawing.Predrawer.Generic_Color
  (Dest : in out Drawer'Class;
   Fig  :        Drawable'Class)
is
   pragma Unreferenced (Fig);
begin
   Dest.Set_Color (Color, Agpl.Constants.Alpha_Opaque);
end Agpl.Drawing.Predrawer.Generic_Color;
