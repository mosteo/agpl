 

-- Encapsulates access to magnet attributes. Not optimized for heavy access.

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Doubly_Linked_Lists;

package Agpl.Magnet is

   Uri_Attr        : constant String := "xt";
   Name_Attr       : constant String := "dn";
   Source_Attr     : constant String := "xs";
   Alt_Source_Attr : constant String := "as";

   User_Defined_Prefix : constant String := "x.";

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Invalid_Url : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is private;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creates from a properly constructed magnet string.
   -- May raise Invalid_Url
   function Create (Url : in String) return Object;

   ------------------------------------------------------------------------
   -- Add_Attribute                                                      --
   ------------------------------------------------------------------------
   -- Appends a new attribute value to the magnet
   procedure Add_Attribute (
      This : in out Object; Attr : in String; Value : in String);

   ------------------------------------------------------------------------
   -- Get_Attribute                                                      --
   ------------------------------------------------------------------------
   -- Get the Nth [named] attribute of the magnet:
   -- Will return "" if no named attr or index out of bounds
   function Get_Attribute (
      This : in Object; 
      Attr : in String   := ""; 
      Pos  : in Positive := 1) return String;

   ------------------------------------------------------------------------
   -- Get_Hash_Type                                                      --
   ------------------------------------------------------------------------
   -- Returns the first xt uri type.
   -- Returns "several" if several uri types available
   function Get_Hash_Type (This : in Object) return String;
   -- Permits enumeration of hash types:
   -- Will raise Constraint_Error when Pos > #hashes
   function Get_Hash_Type (This : in Object; Pos : in Positive) return String;

   ------------------------------------------------------------------------
   -- Get_Hash_Value                                                     --
   ------------------------------------------------------------------------
   -- Returns the hash for the given hash type
   -- or the first hash if none specified
   function Get_Hash_Value (This : in Object; Kind : in String := "") 
      return String;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   -- Returns the dn attribute.
   function Get_Name (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Num_Attributes                                                 --
   ------------------------------------------------------------------------
   -- Of a given type [optionally]
   function Get_Num_Attributes (
      This : in Object;
      Attr : in String := "") return Natural;

   ------------------------------------------------------------------------
   -- Is_Magnet                                                          --
   ------------------------------------------------------------------------
   -- Says if a string is a valid magnet link
   function Is_Magnet (This : in String) return Boolean;

   ------------------------------------------------------------------------
   -- To_String                                                          --
   ------------------------------------------------------------------------
   -- Returns the string form of the magnet
   function To_String (This : in Object) return String;

private

   type Attr_Record is record
      Key, Value : Ustring;
   end record;

   package Attr_List is new Ada.Containers.Doubly_Linked_Lists (Attr_Record, "=");

   type Object is record
      Original_Url   : Ustring;
      Attrs          : Attr_List.List;
   end record;

end Agpl.Magnet;
