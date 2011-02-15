package body Agpl.Statistics.Series.Ordered is

   use Sets;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (This : in out Serie;
      Sample : Number)
   is
   begin
      Statistics.Series.Serie (This).Append (Sample);

      This.Sorted_Data.Insert (Sample);

      if Natural (This.Sorted_Data.Length) = 1 then
         for I in This.Percentiles'Range loop
            This.Percentiles (I) := This.Sorted_Data.First;
         end if;
      end if;

      for I in This.Counters'Range loop
         This.Counters (I) := This.Counters (I) + I;
         if This.Counters (I) >= 100 then
            This.Counters (I) := This.Counters (I) - 100;
            This.percentiles
      end loop;
   end Append;

   ------------
   -- Median --
   ------------

   not overriding function Median
     (This : Serie)
      return Number
   is
   begin
      return Element (This.Percentiles (50));
   end Median;

   ---------------
   -- Percentil --
   ---------------

   not overriding function Percentil
     (This : Serie;
      Nth : Percent)
      return Number
   is
   begin
      return Element (This.Percentiles (Nth));
   end Percentil;

end Agpl.Statistics.Series.Ordered;
