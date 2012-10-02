package body Agpl.Gdk.Controlled_Gc is

   ------------
   -- Get_Gc --
   ------------

   function Get_Gc (This : in Object) return Gdk_GC is
   begin
      return This.Gc;
   end Get_Gc;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
   begin
      Gdk_New (This.Gc, This.Draw);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      Unref (This.Gc);
   end Finalize;

end Agpl.Gdk.Controlled_Gc;
