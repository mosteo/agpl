package Agpl.Colors is

   type Normalized_Component is new Float range 0.0 .. 1.0;

   type Hue        is mod 360;
   type Saturation is new Normalized_Component;
   type Value      is new Normalized_Component;

   type Red        is new Normalized_Component;
   type Green      is new Normalized_Component;
   type Blue       is new Normalized_Component;

   type Alpha      is new Normalized_Component; -- 1.0 => Totally transparent

   type HSV_Color is record
      H : Hue;
      S : Saturation;
      V : Value;
   end record;

   type RGB_Color is record
      R : Red;
      G : Green;
      B : Blue;
   end record;

   function RGB (Color : HSV_Color) return RGB_Color;
   pragma Inline (RGB);

end Agpl.Colors;
