private with Ada.Containers.Doubly_Linked_Lists;

generic
   type Number is digits <>;
package Agpl.Statistics.Series is

   --  Depending on the actual precision of Number, some of the subprograms here
   --  could give slightly off results due to the online computation of moments.
   --  See http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#On-line_algorithm

   pragma Preelaborate;

   type Serie is tagged limited private;

   procedure Append (This : in out Serie; Sample : Number);

   function Length (This : Serie) return Natural;

   function Mean (This : Serie) return Number;

   function Variance (This : Serie) return Number;
   --  This is the *Sample* variance (i.e. / (n - 1)) when n > 1

   function Stdev    (This : Serie) return Number;
   function Sigma    (This : Serie) return Number renames Stdev;
   --  Just Sqrt(Variance)

   type Confidences is (CI_50, CI_68, CI_90, CI_95, CI_99);
   --  Shameful kludge because I'm not up to date with my statistics.

   function Confidence_Interval (This       : Serie;
                                 Confidence : Confidences)
                                 return       Number'Base;
   --  Gives the µ + zσ range, so µ±CI comprises the given percent of values

   subtype Probability is Float range 0.0 .. 1.0;
   function Confidence_Interval (This       : Serie;
                                 Confidence : Probability)
                                 return       Number'Base;
   --  Proper version for any confidence interval

   function Min (This : Serie) return Number;
   function Max (This : Serie) return Number;

private

   pragma Inline (Length, Mean, Variance, Stdev, Confidence_Interval, Min, Max);

   CI_Factor : constant array (Confidences) of Number :=
                 (CI_50 => 0.674,
                  CI_68 => 1.0,
                  CI_90 => 1.645,
                  CI_95 => 1.96,
                  CI_99 => 2.576);

   package Lists is new Ada.Containers.Doubly_Linked_Lists (Number);

   type Serie is tagged limited record
      Data : Lists.List;
      Min  : Number      := Number'Last;
      Max  : Number      := Number'First;
      Avg  : Number      := 0.0;
      Var  : Number'Base := 0.0;

      M_2  : Number'Base := 0.0; -- For online variance computation
   end record;

end Agpl.Statistics.Series;
