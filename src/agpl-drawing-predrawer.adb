package body Agpl.Drawing.Predrawer is

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This :        Object;
      D    : in out Drawer'Class)
   is
   begin
      This.Prepare (D, This.Target.all);
      This.Target.Draw (D);
   end Draw;

end Agpl.Drawing.Predrawer;
