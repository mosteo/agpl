--  with Agpl.Trace; use Agpl.Trace;

package body Agpl.Scaling1d is

   ---------------------
   -- Set_Equivalence --
   ---------------------

   function Set_Equivalence (Ref1x1, Ref1x2,
                             Ref2x1, Ref2x2 : Real)
      return Object
   is
      This : Object;
   begin
      This.R1x1 := Ref1x1;
      This.R1x2 := Ref1x2;

      This.R2x1 := Ref2x1;
      This.R2x2 := Ref2x2;

      This.Ratio := (Ref2x2 - Ref2x1) / (Ref1x2 - Ref1x1);

      return This;
   end Set_Equivalence;

   -------------
   -- Scale_X --
   -------------

   function Scale
     (This  : Object;
      Ref1x : Real)
      return Real
   is
   begin
      return This.R2x1 + (Ref1x - This.R1x1) * This.Ratio;
   end Scale;

   ---------------
   -- Unscale_X --
   ---------------

   function Unscale
     (This  : Object;
      Ref2x : Real)
      return Real
   is
   begin
      return This.R1x1 + (Ref2x - This.R2x1) / This.Ratio;
   end Unscale;

end Agpl.Scaling1d;
