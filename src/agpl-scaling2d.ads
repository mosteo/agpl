--  Simple scaling transformation

generic
   type Real is digits <>;
package Agpl.Scaling2d is

   pragma Pure;

   --  I was dumb or numb when I did this, since functionality is duplicated
   --  when two objects would do the same for a single scale each.

   type Object is tagged private;

   function Set_Equivalence (Ref1x1, Ref1x2,
                             Ref1y1, ref1y2,
                             Ref2x1, Ref2x2,
                             Ref2y1, ref2y2 : Real) return Object;
   --  Say equivalent X and Y in two reference frames.

   function Scale_X (This  : Object;
                     Ref1x : Real) return Real;
   function Scale_Y (This  : Object;
                     Ref1y : Real) return Real;

   function Unscale_X (This  : Object;
                       Ref2x : Real) return Real;
   function Unscale_Y (This  : Object;
                       Ref2y : Real) return Real;

private

   type Object is tagged record
      R1x1, R1x2,
      R1y1, R1y2,
      R2x1, R2x2,
      R2y1, r2y2 : Real;

      Ratio_X, Ratio_Y   : Real;
   end record;

end Agpl.Scaling2d;
