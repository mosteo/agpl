with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Agpl.Strings; use Agpl.Strings;

package body Agpl.Reflection is

   type Datum_Access is access all Datum'Class;

   package Value_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Datum_Access);

   protected Values is
      procedure Add (Name : String; D : in out Datum'Class);
      procedure Set (Name, Value : String);
      function  Get (Name        : String) return String;
      function Get_All return Value_Maps.Map;
   private
      Map : Value_Maps.Map;
   end Values;

   ------------
   -- Values --
   ------------

   protected body Values is

   ---------
   -- Add --
   ---------

      procedure Add (Name : String; D : in out Datum'Class) is
      begin
         Map.Include (To_Lower (Name), D'Unchecked_Access);
      end Add;

      ---------
      -- Set --
      ---------

      procedure Set (Name, Value : String) is
      begin
         Map.Element (To_Lower (Name)).all.Set (Value);
      end Set;

      ---------
      -- Get --
      ---------

      function  Get (Name : String) return String is
      begin
         return Map.Element (To_Lower (Name)).Get;
      end Get;

      -------------
      -- Get_All --
      -------------

      function Get_All return Value_Maps.Map is
      begin
         return Map;
      end Get_All;

   end Values;

   ----------
   -- Base --
   ----------

   package body Base is

      ---------
      -- Set --
      ---------

      procedure Set (D : in out Object; Val : String) is
      begin
         D.Val.Set (Value (Val));
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (D : in out Object; Val : Basetype) is
      begin
         D.Val.Set (Val);
      end Set;

      ---------
      -- Get --
      ---------

      function Get (D : Object) return String is
      begin
         return Image (D.Val.Get);
      end Get;

      -----------
      -- Value --
      -----------

      function Value (Name : String;
                      B    : Basetype) return Object is
      begin
         return O : Object (new String'(Name)) do
            O.Val.Set (B);
         end return;
      end Value;

      -----------
      -- Value --
      -----------

      function Value (D : Object) return Basetype is
      begin
         return D.Val.Get;
      end Value;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (D : in out Object) is
      begin
         Register (D.Name.all, D);
      end Initialize;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (D : in out Object) is
         procedure Free is new Ada.Unchecked_Deallocation (String,
                                                           Name_Access);
         X : Name_Access := D.Name;
      begin
         Free (X);
      end Finalize;

   end Base;

   --------------
   -- Register --
   --------------

   procedure Register (Name : String; D : in out Datum'Class) is
   begin
      Values.Add (Name, D);
   end Register;

   ---------
   -- Set --
   ---------

   procedure Set (Name, Value : String) is
   begin
      Values.Set (Name, Value);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return String is
   begin
      return Values.Get (Name);
   end Get;

   -------------
   -- Get_All --
   -------------

   function  Get_All return Containers.String_String_Maps.Map is
      Result : Containers.String_String_Maps.Map;
      use Value_Maps;
      procedure Add (I : Cursor) is
      begin
         Result.Insert (Key (I), Element (I).Get);
      end Add;
   begin
      Values.Get_All.Iterate (Add'Access);
      return Result;
   end Get_All;

end Agpl.Reflection;
