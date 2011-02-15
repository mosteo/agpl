private with Ada.Containers.Doubly_Linked_Lists;
with Agpl.Numbers;

generic
   with package Base_Number is new Agpl.Numbers (<>);
package Agpl.Statistics.Series is

   --  Depending on the actual precision of Number, some of the subprograms here
   --  could give slightly off results due to the caching use for fast results.
   --  In order to prevent this, there's a subprogram that recomputes everything
   --    in time O(n).

   pragma Preelaborate;

   use Base_Number;

   type Serie is tagged limited private;

   procedure Append (This : in out Serie; Sample : Number);

   function Mean (This : Serie) return Number;

   function Min (This : Serie) return Number;
   function Max (This : Serie) return Number;

private

   use Base_Number;

   package Lists is new Ada.Containers.Doubly_Linked_Lists (Number);

   type Serie is tagged limited record
      Data : Lists.List;
      Len  : Number := To_Number (0);
      Min  : Number := Last;
      Max  : Number := First;
      Avg  : Number := To_Number (0);
   end record;

end Agpl.Statistics.Series;
