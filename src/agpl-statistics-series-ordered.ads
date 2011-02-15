private with Ada.Containers.Ordered_Multisets;

generic
package Agpl.Statistics.Series.Ordered is

   pragma Preelaborate;

   type Serie is new Series.Serie with private;

   --  Here the data is ordered, which gives access to some other moments.
   --  Slightly more expensive than regular Series, insertion is O(log n).
   --  Still, query is O(1)

   overriding
   procedure Append (This : in out Serie; Sample : Number);

   not overriding
   function Median (This : Serie) return Number;

   subtype Percent is Natural range 0 .. 100;

   not overriding
   function Percentil (This : Serie; Nth : Percent) return Number;

private

   package Sets is new Ada.Containers.Ordered_Multisets (Number);

   type Cursor_Array  is array (Percent) of Sets.Cursor;
   type Natural_Array is array (Percent) of Natural;

   type Serie is new Series.Serie with record
      Sorted_Data : Sets.Set;
      Percentiles : Cursor_Array  := (others => Sets.No_Element);
      Counters    : Natural_Array := (others => 0);
      --  These counters are used to know when a Percentil has to be advanced.
   end record;
   --  Do the necessary computations for the statistics provided

end Agpl.Statistics.Series.Ordered;
