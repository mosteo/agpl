
package body Agpl.Gdk.Conversions is

   --------------
   -- To_Float --
   --------------

   function To_Float (N : Float) return Float is
   begin
      return N;
   end To_Float;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (N : Integer) return Gint is
   begin
      return Gint (N);
   end To_Gint;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (N : Float) return Gint is
   begin
      return Gint (N);
   end To_Gint;

end Agpl.Gdk.Conversions;
