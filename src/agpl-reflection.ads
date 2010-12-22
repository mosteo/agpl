with Ada.Finalization;
with Agpl.Containers.String_String_Maps;
with Agpl.Protected_Value;

package Agpl.Reflection is

   --  See child Booleans package for an example of usage...
   --  Note that accessed values should be at library level...

   pragma Preelaborate;

   procedure Set (Name,
                  Value : String);
   --  Use to set a reflected variable

   function  Get (Name  : String)
                  return  String;
   --  Use to get a reflecte string representation

   type Datum is limited interface;

   procedure Set (D : in out Datum; Value : String) is abstract;
   function  Get (D :        Datum)  return String  is abstract;
   function  Get_All return Containers.String_String_Maps.Map;

   generic
      type Basetype is private;
      with function Value (B : String)   return Basetype is <>;
      with function Image (B : Basetype) return String   is <>;
   package Base is

      --  These types are thread safe by default... protected and heavyweight.

      type Name_Access is access String;

      type Object (Name  : Name_Access) is limited
      new Ada.Finalization.Limited_Controlled
        and Datum with private;

      overriding
      procedure Set (D : in out Object; Val : String);

      not overriding
      procedure Set (D : in out Object; Val : Basetype);

      overriding
      function  Get (D : Object)  return String;

      not overriding
      function Value (Name : String;
                      B    : Basetype) return Object;

      not overriding
      function Value (D : Object) return Basetype;

      not overriding
      function Image (D : Object) return String renames Get;

   private

      package Safe is new Agpl.Protected_Value (Basetype);

      type Object (Name  : Name_Access) is limited
        new Ada.Finalization.Limited_Controlled
        and Datum
      with record
         Val : Safe.Object;
      end record;

      overriding procedure Initialize (D : in out Object);
      overriding procedure Finalize   (D : in out Object);

   end Base;

private

   procedure Register (Name : String; D : in out Datum'Class);

end Agpl.Reflection;
