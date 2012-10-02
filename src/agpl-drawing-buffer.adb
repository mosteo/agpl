with Agpl.Trace; use Agpl.Trace;

package body Agpl.Drawing.Buffer is

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (This   : in out Object;
      X1, Y1,
      X2, Y2 : Float)
   is
   begin
      This.Actions.Append (Action_Line'(X1, Y1, X2, Y2));
   end Draw_Line;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (This   : in out Object;
      X1, Y1,
      X2, Y2 : Float) is
   begin
      This.Actions.Append (Action_Fill'(X1, Y1, X2, Y2));
   end Fill_Rectangle;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (This  : in out Object;
      Rgb   :        Types.Rgb_triplet;
      Alpha :        Types.Unsigned_8)
   is
   begin
      This.Actions.Append (Action_Color'(Rgb, Alpha));
   end Set_Color;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Object;
                    X, Y : Float;
                    Utf8 : String)
   is
   begin
      This.Actions.Append (Action_Write'(Utf8'Length, X, Y, Utf8));
   end Write;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (This  :        Object;
      Dest  : in out Drawer'Class)
   is
      use Action_Lists;
      procedure Flush (I : Cursor) is
      begin
         Element (I).Perform (Dest);
      end Flush;
   begin
      This.Actions.Iterate (Flush'Access);
   end Flush;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      This.Actions.Clear;
   end Clear;

   ---------------------
   -- Flush_And_Clear --
   ---------------------

   procedure Flush_And_Clear (This  : in out Object;
                              Dest  : in out Drawer'Class) is
   begin
      This.Flush (Dest);
      This.Clear;
   end Flush_And_Clear;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Object;
                   Dest : in out Drawer'Class) is
   begin
      This.Flush (Dest);
   end Draw;

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This :        Action_Line;
      Dest : in out Drawer'Class)
   is
   begin
      Dest.Draw_Line (This.X1, This.Y1, This.X2, This.Y2);
   end Perform;

   -------------
   -- Perform --
   -------------

   procedure Perform (This :        Action_Fill;
                      Dest : in out Drawer'Class) is
   begin
      Dest.Fill_Rectangle (This.X1, This.Y1, This.X2, This.Y2);
   end Perform;

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This :        Action_Color;
      Dest : in out Drawer'Class)
   is
   begin
      Dest.Set_Color (This.Rgb, This.Alpha);
   end Perform;

   -------------
   -- Perform --
   -------------

   procedure Perform (This :        Action_Write;
                      Dest : in out Drawer'Class)
   is
   begin
      Dest.Write (This.X, This.Y, This.Utf8);
   end Perform;

end Agpl.Drawing.Buffer;
