private with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;

generic
   type Index (<>) is private;
   type Value (<>) is private;
   with function "<" (L, R : Index) return Boolean is <>;
package Agpl.Containers.Indefinite_Matrix2d is

   --  This is a sparse 2D matrix
   --  Care has been taken to make this type as efficient as possible given
   --  the Ada.Containers it internally uses.
   --  This type is *not* thread-safe.

   pragma Preelaborate;

   package Index_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Index);

   subtype Index_Vector is Index_Vectors.Vector;

   type Matrix is tagged private;

   function Contains (This : Matrix; R, C : Index) return Boolean;

   function Get (This : Matrix; R, C : Index) return Value;

   procedure Get (This : Matrix; R, C : Index; V : out Value; Found : out Boolean);
   --  If found, V will be valid

   procedure Set (This : in out Matrix; R, C : Index; V : Value);

   function Row_Indexes (This : Matrix) return Index_Vector;
   --  O(n)



   function Column_Indexes (This : Matrix) return Index_Vector;
   --  All found in all rows
   --  O(n**2)

private

   package Index_Value_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Index, Value);

   use type Index_Value_Maps.Map;

   package Index_Index_Value_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Index, Index_Value_Maps.Map);

   type Matrix is new Index_Index_Value_Maps.Map with null record;

end Agpl.Containers.Indefinite_Matrix2d;
