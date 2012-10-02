with Agpl.Trace; use Agpl.Trace;

with Gdk.GC;     use Gdk.GC;

package body Agpl.Gdk.Utils is

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (This     : in Controlled_Gc.Object;
      Color    : in Gdk_Color;
      Drawable : in Gdk_Drawable)
   is
      use type Controlled_Gc.Object;
      Ok : Boolean;
      C  : Gdk_Color := Color;
   begin
      Alloc_Color (Get_Colormap (Drawable), C, Success => Ok, Best_Match => False);
      pragma Assert (Ok);
      Set_Foreground (+This, C);
   exception
      when E : others =>
         Log ("Gdk.Utils.Set_Color: " & Report (E), Warning);
   end Set_Color;

end Agpl.Gdk.Utils;
