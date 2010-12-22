
with Ada.Streams;
with Agpl.Types;

package Agpl.Conversions is

   pragma Preelaborate;

   --  A two char string
   function From_Hex (C : Types.Hex_Character) return Character;
   function From_Hex (S : String) return String;

   function To_Hex (C : in Character) return Types.Hex_Character;
   function To_Hex (S : in String) return String;
   function To_Hex (I : in Natural; Length : in Natural := 0) return String;
   --  Return the hex representation, 0-left-padded to use at most Length chars
   function To_Hex (S : Ada.Streams.Stream_Element_Array) return String;

   function To_Stream (S : String) return Ada.Streams.Stream_Element_Array;
   --  Gets an hex rep of a stream (for To_Hex) and put it back into array.

   function To_String (N : Integer) return String;
   function         S (N : Integer) return String renames To_String;

   --  Works as 'Img but removes leading/trailing spaces
   --  Performs rounding on floats
   --  Not particularly fast, since they rely on Long_Long_Float'Image
   function To_string (N        : Float;
                       Decimals : Natural := 2)
                       return     String;
   function         S (N        : Float;
                       Decimals : Natural := 2)
                       return     String renames To_String;

   function To_string (N        : Long_Long_Float;
                       Decimals : Natural := 2)
                       return     String;
   function         S (N        : Long_Long_Float;
                       Decimals : Natural := 2)
                       return     String renames To_String;

   generic
      type Real is delta <> digits <>;
   function Decimal_To_Str (N        : Real;
                            Decimals : Natural := 2)
                            return     String;

   --  All the following, in the end, convert to long_long_float.
   --  So may be there's precision loss? Should be fixed?
   generic
      type Real is digits <>;
   function To_Str (N        : Real;
                    Decimals : Natural := 2)
                    return     String;

   generic
      type Real is delta <>;
   function Fixed_To_Str (N        : Real;
                          Decimals : Natural := 2)
                          return     String;

   function Trim (This : in String) return String;
--   pragma Inline (Trim);

   ------------------------------------------------------------------------
   -- To_size                                                            --
   ------------------------------------------------------------------------
   --  Beautifies some quantity (bytes) appending the correct units
   function To_size (
      Qty : in Integer; Decimals : in Natural := 2) return String;

   ------------------------------------------------------------------------
   -- To_size                                                            --
   ------------------------------------------------------------------------
   --  Beautifies some quantity (bytes) appending the correct units
   --  Float flavor
   function To_size (
      Qty : in Float; Decimals : in Natural := 2) return String;


end Agpl.Conversions;
