private with Agpl.Chronos;

generic
   type Real is digits <>;
package Agpl.Timed_Transformer is

   type Object is tagged private;

   function Create (Val : Real) return Object;

   procedure Set (This : in out Object;
                  Val    :        Real;
                  Period :        Duration);
   --  The new value we want, to be reached after period in linear fashion.

   function Value (This : Object) return Real;
   --  Get the current value

private

   type Object is tagged record
      Timer  : Agpl.Chronos.Object;
      Val    : Real     := 0.0;
      Old    : Real     := 0.0;
      Period : Duration := 0.0;
   end record;

end Agpl.Timed_Transformer;
