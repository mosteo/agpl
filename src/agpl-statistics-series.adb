package body Agpl.Statistics.Series is

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Serie; Sample : Number) is
      One : constant Number := To_Number (1);
   begin
      This.Data.Append (Sample);

      This.Avg := (This.Avg * This.Len + Sample) / (This.Len + One);
      This.Len := This.Len + One;

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

end Agpl.Statistics.Series;
