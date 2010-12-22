with Ada.Finalization;
with Agpl.Interfaces.C.Arrays;

with Interfaces.C;
with Interfaces.C.Strings;

package Agpl.Interfaces.C.Types is

   --  Types for interfacing with C/C++ commonly used across AGPL

   pragma Preelaborate;

   package Ic renames Standard.Interfaces.C;

   type Return_Code      is new Ic.Int;

   Return_Ok  : constant Return_Code := 0;
   Return_Err : constant Return_Code := 1;

   type Agpl_Bool        is new Ic.Int;

   False : constant Agpl_Bool := 0;
   True  : constant Agpl_Bool := 1;

   function "+" (B : Boolean)   return Agpl_Bool;
   function "+" (B : Agpl_Bool) return Boolean;

   type Double           is new Ic.Double;
   type Int              is new Ic.Int;
   subtype Chars_Ptr     is Ic.Strings.Chars_Ptr;
   subtype Char_Array    is Ic.Char_Array;

   package Int_Arrays    is new Arrays (Int);
   package Double_Arrays is new Arrays (Double);

   type Void is private;

   type Void_Ptr is access all Void;
   pragma Convention (C, Void_Ptr);
   pragma No_Strict_Aliasing (Void_Ptr);

   type Void_Ptr_Nonlimited is new Ada.Finalization.Controlled with record
      Ptr : Void_Ptr;
   end record;
   --  Note that default copy is a shallow copy!!!

   type Void_Ptr_Limited is new Ada.Finalization.Limited_Controlled with record
      Ptr : Void_Ptr;
   end record;

   type Cstring is tagged private;
   --  A controlled layer over chars_ptr

   function New_Cstring (Str : String) return Cstring;
   function New_Empty_Cstring (Length : Natural) return Cstring;
   --  Uninitialized of Length characters + 1 for \0
   function "+"         (Str : String) return Cstring renames New_Cstring;
   function Ptr         (Cstr : Cstring) return Ic.Strings.Chars_Ptr;
   pragma Inline (New_Cstring, "+", Ptr);
   function Value       (Str : Cstring) return String;
   function "-"         (Str : Cstring) return String renames Value;

private

   type Void is new Standard.Interfaces.C.Int;

   type Cstring is new Ada.Finalization.Controlled with record
      Ptr : Ic.Strings.Chars_Ptr;
   end record;

   overriding
   procedure Adjust (This : in out Cstring);

   overriding
   procedure Finalize (This : in out Cstring);

end Agpl.Interfaces.C.Types;
