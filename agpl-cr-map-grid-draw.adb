procedure Agpl.Cr.Map.Grid.Draw
  (This    : in     Object;
   Into    : in out Gdk.Drawer.Object;
   Palette : not null access function (Obs : in Observations'Class)
                                       return   Gdk_Gc)
is

   ---------------
   -- Draw_Cell --
   ---------------

   procedure Draw_Cell (I : in Cursor) is
   begin
      null;
   end Draw_Cell;

begin
   This.Cells.Iterate (Draw_Cell'Access);
end Agpl.Cr.Map.Grid.Draw;

