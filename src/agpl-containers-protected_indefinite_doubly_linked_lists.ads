with Ada.Containers;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Agpl.Containers.Protected_Indefinite_Doubly_Linked_Lists is

   pragma Preelaborate;

   subtype Count_Type is Ada.Containers.Count_Type;

   --  This is not to be used!! Should be hidden :'(
   package Container is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type, "=");

   protected type Protected_List is

      procedure Clear;

      procedure Append
        (New_Item  : Element_Type;
         Count     : Count_Type := 1);

      procedure Delete_First (Count : Count_Type := 1);

      function First_Element return Element_Type;
      --  Exception if empty

      entry First_Element_Blocking (Item : out Element_Type);
      --  Block until some element available
      --  Doesn't delete it.

      entry First_Element_Blocking_And_Delete (Item : out Element_Type);

      function Is_Empty return Boolean;

      function Length return Count_Type;

   private
      Internal : Container.List;
   end Protected_List;

end Agpl.Containers.Protected_Indefinite_Doubly_Linked_Lists;
