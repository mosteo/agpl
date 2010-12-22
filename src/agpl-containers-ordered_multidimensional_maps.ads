with Ada.Containers.Ordered_Maps;

generic
   type Dimensions is (<>);
   type Key_Type is private;
   type Element_Type is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Agpl.Containers.Ordered_Multidimensional_Maps is

   type Key_Array is array (Dimensions) of Key_Type;

   function "<" (L, R : Key_Array) return Boolean;

   package Base_Maps is new Ada.Containers.Ordered_Maps
     (Key_Array, Element_Type, "<", "=");

   subtype Map is Base_Maps.Map;

end Agpl.Containers.Ordered_Multidimensional_Maps;
