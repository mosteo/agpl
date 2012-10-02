
with Gdk.Color; use Gdk.Color;

package Agpl.Gdk.Constants is

   pragma Elaborate_Body;

   FFFF : constant := 2 ** 16 - 1; -- 65535
   FF   : constant := 2 ** 8 - 1; -- 255

   function Black  return Gdk_Color;
   function Blue   return Gdk_Color;
   function Gray   return Gdk_Color;
   function Green  return Gdk_Color;
   function Red    return Gdk_Color;
   function Silver return Gdk_Color;
   function White  return Gdk_Color;

   function Light_Green return Gdk_Color;
   function Light_Red   return Gdk_Color;

   package Colors is
      Black : constant String := "#000000";
      Blue  : constant String := "#0000ff";
      White : constant String := "#ffffff";
   end Colors;

end Agpl.Gdk.Constants;
