--  with Agpl.Trace; use Agpl.Trace;

package body Agpl.Scaling2d is

   type Prn is delta 0.01 digits 10;

   ---------------------
   -- Set_Equivalence --
   ---------------------

   function Set_Equivalence (Ref1x1, Ref1x2,
                             Ref1y1, ref1y2,
                             Ref2x1, Ref2x2,
                             Ref2y1, ref2y2 : Real)
      return Object
   is
      This : Object;
   begin
      This.R1x1 := Ref1x1;
      This.R1x2 := Ref1x2;

      This.R1y1 := Ref1y1;
      This.R1y2 := Ref1y2;

      This.R2x1 := Ref2x1;
      This.R2x2 := Ref2x2;

      This.R2y1 := Ref2y1;
      This.R2y2 := Ref2y2;

      This.Ratio_X := (Ref2x2 - Ref2x1) / (Ref1x2 - Ref1x1);
      This.Ratio_Y := (Ref2y2 - Ref2y1) / (Ref1y2 - Ref1y1);

      return This;
   end Set_Equivalence;

   -------------
   -- Scale_X --
   -------------

   function Scale_X
     (This  : Object;
      Ref1x : Real)
      return Real
   is
   begin
--        Log ("In X:" & Prn (Ref1x)'Img, Always);
--        Log ("Ou X:" & Prn (This.R2x1 + (Ref1x - This.R1x1) * This.Ratio_X)'Img, Always);
      return This.R2x1 + (Ref1x - This.R1x1) * This.Ratio_X;
   end Scale_X;

   -------------
   -- Scale_Y --
   -------------

   function Scale_Y
     (This  : Object;
      Ref1y : Real)
      return Real
   is
   begin
      return This.R2y1 + (Ref1y - This.R1y1) * This.Ratio_Y;
   end Scale_Y;

   ---------------
   -- Unscale_X --
   ---------------

   function Unscale_X
     (This  : Object;
      Ref2x : Real)
      return Real
   is
   begin
      return This.R1x1 + (Ref2x - This.R2x1) / This.Ratio_X;
   end Unscale_X;

   ---------------
   -- Unscale_Y --
   ---------------

   function Unscale_Y
     (This  : Object;
      Ref2y : Real)
      return Real
   is
   begin
      return This.R1y1 + (Ref2y - This.R2y1) / This.Ratio_Y;
   end Unscale_Y;

end Agpl.Scaling2d;
