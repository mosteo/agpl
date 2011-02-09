package body Agpl.Colors is

   subtype NC is Normalized_Component;

   ---------
   -- RGB --
   ---------

   function RGB (Color : HSV_Color) return RGB_Color is
   --  See:
   --  http://code.activestate.com/recipes/576554-covert-color-space-from-hsv-to-rgb-and-rgb-to-hsv/

      Huefix : constant Hue range 0 .. 5     := Color.H / 60;
      F      : constant NC := NC'Base (Color.H) / 60.0 - NC'Base (Huefix);
      P      : constant NC := NC (Color.V) *  (1.0 -      NC (Color.S));
      Q      : constant NC := NC (Color.V) *  (1.0 - F  * NC (Color.S));
      T      : constant NC := NC (Color.V) * ((1.0 - F) * NC (Color.S));
   begin
      case Huefix is
         when 0 =>
            return (Red (Color.V), Green (T),       Blue (P));
         when 1 =>
            return (Red (Q),       Green (Color.V), Blue (P));
         when 2 =>
            return (Red (P),       Green (Color.V), Blue (T));
         when 3 =>
            return (Red (P),       Green (Q),       Blue (Color.V));
         when 4 =>
            return (Red (T),       Green (P),       Blue (Color.V));
         when 5 =>
            return (Red (Color.V), Green (P),       Blue (Q));
      end case;
   end RGB;

end Agpl.Colors;
