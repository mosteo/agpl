--  Simple scaling transformation

generic
   type Real is digits <>;
package Agpl.Scaling1d is

   pragma Pure;

   type Object is tagged private;

   function Set_Equivalence (Ref1x1, Ref1x2,
                             Ref2x1, Ref2x2 : Real) return Object;
   --  Say equivalent X in two reference frames.

   function Scale (This  : Object;
                   Ref1x : Real) return Real;
   --  Pass from ref1 to ref2

   function Unscale (This  : Object;
                     Ref2x : Real) return Real;
   --  Pass from ref2 to ref1

private

   type Object is tagged record
      R1x1, R1x2 : Real;
      R2x1, R2x2 : Real;

      Ratio      : Real;
   end record;

end Agpl.Scaling1d;
