private with Agpl.Ustrings;

package Agpl.Unique_Id is

   pragma Preelaborate;

   type Object is tagged private;

   function Value (This : String) return Object;
   function "+"   (This : String) return Object renames Value;

   procedure Set (This : in out Object; Id : String);

   function Image (This : Object) return String;
   function "+"   (This : Object) return String renames Image;

   type Univocally_Identifiable is interface;

   function Id (This : Univocally_Identifiable) return String is abstract;

private

   use Agpl.Ustrings;

   type Object is tagged record
      Id : Ustring;
   end record;

   pragma Inline (Value, Image);

end Agpl.Unique_Id;
