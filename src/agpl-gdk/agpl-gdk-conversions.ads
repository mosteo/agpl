
with Glib; use Glib;

package Agpl.Gdk.Conversions is

   pragma Preelaborate;

   function To_Float (N : Float) return Float;
   function To_Gint (N : Integer) return Gint;
   function To_Gint (N : Float) return Gint;

private

   pragma Inline (To_Float, To_Gint);

end Agpl.Gdk.Conversions;
