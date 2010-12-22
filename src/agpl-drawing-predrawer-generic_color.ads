with Agpl.Types;

generic
   Color : Agpl.Types.Rgb_Triplet;
procedure Agpl.Drawing.Predrawer.Generic_Color
  (Dest : in out Drawer'Class;
   Fig  :        Drawable'Class);

pragma Preelaborate (Agpl.Drawing.Predrawer.Generic_Color);
