
with Agpl.Gdk.Controlled_Gc;

with Gdk.Color; use Gdk.Color;
with Gdk.Drawable; use Gdk.Drawable;

package Agpl.Gdk.Utils is

   --   pragma Elaborate_Body;

   procedure Set_Color (This     : in Controlled_Gc.Object;
                        Color    : in Gdk_Color;
                        Drawable : in Gdk_Drawable);
   --  The color will be allocated from default color map.

end Agpl.Gdk.Utils;
