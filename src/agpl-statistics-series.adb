with Ada.Numerics.Generic_Elementary_Functions;

with Agpl.Interfaces.C.Types;
with Agpl.Conversions;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Constants;

package body Agpl.Statistics.Series is

   use type Ada.Containers.Count_Type;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Number'Base);
   use Math;

   function S is new Conversions.To_Str (Number'Base);

   function Drawable (This   : Serie;
                      Conf   : Probability := 0.95;
                      Width  : Float       := 0.1)
                      return   Drawing.Buffer.Object
   is
      use Types.Constants;
      D : Drawing.Buffer.Object;
      W : constant Float := Float (Number'Last - Number'First) * Width / 2.0;
   begin
--        Log ("Min/5/avg/var/conf/95/Max: " &
--             S (This.Min) & "/" &
--             S (This.Avg - This.Confidence_Interval (Conf)) & "/" &
--             S (This.Avg) & "/" &
--             S (This.Var) & "/" &
--             S (This.Confidence_Interval (Conf)) & "/" &
--             S (This.Avg + This.Confidence_Interval (Conf)) & "/" &
--             S (This.Max),
--             Always);

      --  Total range
      D.Set_Color (Blue, Alpha_Opaque);
      D.Draw_Line (0.0, Float (Number'First),
                   0.0, Float (Number'Last));

      --  Confidence interval
      D.Set_Color (Blue, Alpha_Opaque);
      D.Draw_Rectangle (-W, Float (This.Avg + This.Confidence_Interval (Conf)),
                        +W, Float (This.Avg - This.Confidence_Interval (Conf)));

      --  Min/Avg/Max whiskers
      D.Set_Color (Red, Alpha_Opaque);
      D.Draw_Line (-W, Float (This.Avg), +W, Float (This.Avg));
      D.Draw_Line (-W, Float (This.Min), +W, Float (This.Min));
      D.Draw_Line (-W, Float (This.Max), +W, Float (This.Max));

      return D;
   end Drawable;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Serie; Sample : Number) is
      Diff : constant Number'Base := Sample - This.Avg;
   begin
      This.Data.Append (Sample);

      This.Avg := This.Avg + Diff / Number'Base (This.Data.Length);
      This.M_2 := This.M_2 + Diff * (Sample - This.Avg);

      if Natural (This.Data.Length) <= 1 then
         This.Var := This.M_2;
      else
         This.Var := This.M_2 / Number'Base (This.Data.Length - 1);
      end if;

      if Sample < This.Min then
         This.Min := Sample;
      end if;
      if Sample > This.Max then
         This.Max := Sample;
      end if;
   end Append;

   ----------
   -- Mean --
   ----------

   function Mean (This : Serie) return Number is
   begin
      if This.Data.Is_Empty then
         raise Constraint_Error with "No data to compute average.";
      else
         return This.Avg;
      end if;
   end Mean;

   --------------
   -- Variance --
   --------------

   function Variance (This : Serie) return Number'Base is
   begin
      if This.Data.Is_Empty then
         raise Constraint_Error with "Insufficient data for variance";
      else
         return This.Var;
      end if;
   end Variance;

   -----------
   -- Stdev --
   -----------

   function Stdev (This : Serie) return Number'Base is
   begin
      return Sqrt (This.Var);
   end Stdev;

   ------------
   -- Stderr --
   ------------

   function Stderr   (This : Serie) return Number'Base is
   begin
      return This.Stdev / Sqrt (Number'Base (This.Data.Length));
   end Stderr;

   ---------
   -- Min --
   ---------

   function Min (This : Serie) return Number is
   begin
      return This.Min;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (This : Serie) return Number is
   begin
      return This.Max;
   end Max;

   -------------------------
   -- Confidence_Interval --
   -------------------------

   function Confidence_Interval (This       : Serie;
                                 Confidence : Confidences)
                                 return       Number'Base is
   begin
      return CI_Factor (Confidence) * This.Stderr;
   end Confidence_Interval;

   -------------------------
   -- Confidence_Interval --
   -------------------------

   function Confidence_Interval (This       : Serie;
                                 Confidence : Probability)
                                 return       Number'Base
   is
      use Agpl.Interfaces;

      function Cdf_Chi_P_Inv (P  : C.Types.Double;
                              Nu : C.Types.Double) return C.Types.Double;
      --  P : Desired confidence
      --  Nu: Degrees of freedom
      pragma Import (C, Cdf_Chi_P_Inv, "gsl_cdf_chisq_Pinv");
      --  This requires both GSL and BLAS
   begin
      return
        Number'Base (Cdf_Chi_P_Inv (C.Types.Double (Confidence), 1.0)) *
        This.Stderr;
   end Confidence_Interval;

   ------------
   -- Length --
   ------------

   function Length (This : Serie) return Natural is
   begin
      return Natural (This.Data.Length);
   end Length;

end Agpl.Statistics.Series;
