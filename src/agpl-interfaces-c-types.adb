with Ada.Strings.Fixed;

package body Agpl.Interfaces.C.Types is

   ---------
   -- "+" --
   ---------

   function "+" (B : Boolean)   return Agpl_Bool is
   begin
      if B then
         return 1;
      else
         return 0;
      end if;
   end "+";

   function "+" (B : Agpl_Bool) return Boolean is
   begin
      return B /= 0;
   end "+";

   -----------------
   -- New_Cstring --
   -----------------

   function New_Cstring (Str : String) return Cstring is
   begin
      return Cstring'(Ada.Finalization.Controlled with
                      Ptr => Ic.Strings.New_String (Str));
   end New_Cstring;

   -----------------------
   -- New_Empty_CString --
   -----------------------

   function New_Empty_CString (Length : Natural) return Cstring is
      use Ada.Strings.Fixed;
   begin
      return New_Cstring ((Length + 1) * Character'Val (0));
   end New_Empty_CString;

   ---------
   -- Ptr --
   ---------

   function Ptr (Cstr : Cstring) return Ic.Strings.Chars_Ptr is
   begin
      return Cstr.Ptr;
   end Ptr;

   -----------
   -- Value --
   -----------

   function Value (Str : Cstring) return String is
   begin
      return Standard.Interfaces.C.Strings.Value (Str.Ptr);
   end Value;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (This : in out Cstring)
   is
      use Ic.Strings;
   begin
      if This.Ptr /= Null_Ptr then
         Ic.Strings.Free (This.Ptr);
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Cstring) is
      use Ic.Strings;
   begin
      if This.Ptr /= Null_Ptr then
         This.Ptr := New_String (Value (This.Ptr));
      end if;
   end Adjust;

end Agpl.Interfaces.C.Types;
