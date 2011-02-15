generic
   type Number is private;
   First : Number;
   Last  : Number; -- These two define the range of valid values
   with function "<" (L, R : Number) return Boolean is <>;
   with function "=" (L, R : Number) return Boolean is <>;
   with function "+" (L, R : Number) return Number is <>;
   with function "-" (L, R : Number) return Number is <>;
   with function "*" (L, R : Number) return Number is <>;
   with function "/" (L, R : Number) return Number is <>;
   with function To_Number (F : Float) return Number is <>;
   with function To_Number (I : Integer) return Number is <>;
package Agpl.Numbers is

   pragma Pure;

   --  Base package for number manipulation generic packages
   --  Depending on the precision of Number... bad things can happen?

end Agpl.Numbers;
