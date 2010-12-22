--  with Agpl.Strings;

package body Agpl.Drawing is

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (This   : in out Drawer'Class;
      X1, Y1,
      X2, Y2 : Float)
   is
   begin
      This.Draw_Line (X1, Y1, X1, Y2);
      This.Draw_Line (X1, Y2, X2, Y2);
      This.Draw_Line (X2, Y2, X2, Y1);
      This.Draw_Line (X2, Y1, X1, Y1);
   end Draw_Rectangle;

end Agpl.Drawing;
