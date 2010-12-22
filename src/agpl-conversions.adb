--  with Ada.Long_Long_Float_Text_Io;
with Ada.Strings;
with Ada.Strings.Fixed;

with Agpl.Strings;        use Agpl.Strings;
with Agpl.Strings.Fields;
--  with Agpl.Trace; use Agpl.Trace;

pragma Warnings (Off);
with System.Img_Real;
pragma Warnings (On);
--  Necessary evil to remain preelaborable and not lose precision using
--   decimal types when imaging floats without E notation.

with Interfaces;

package body Agpl.Conversions is

   ------------------------------------------------------------------------
   -- Trim                                                               --
   ------------------------------------------------------------------------
   function Trim (This : in String) return String is
   begin
      return Ada.Strings.Fixed.Trim (This, Ada.Strings.Both);
   end Trim;

   ------------------------------------------------------------------------
   -- From_Hex                                                           --
   ------------------------------------------------------------------------
   FHex : constant array (Character) of Interfaces.Unsigned_8 := (
         '0' => 0,  '1' => 1,  '2' => 2,  '3' => 3,  '4' => 4,
         '5' => 5,  '6' => 6,  '7' => 7,  '8' => 8,  '9' => 9,
         'a' => 10, 'A' => 10, 'b' => 11, 'B' => 11, 'c' => 12, 'C' => 12,
         'd' => 13, 'D' => 13, 'e' => 14, 'E' => 14, 'f' => 15, 'F' => 15,
         others => 255);
   function From_Hex (C : Types.Hex_Character) return Character is
      use Interfaces;
   begin
      return Character'Val (
         Shift_Left (FHex (C (C'First)), 4) or FHex (C (C'Last)));
   end From_hex;

   function From_Hex (S : String) return String is
      R : String (1 .. S'Length / 2);
      P : Natural := S'First;
   begin
      for N in R'Range loop
         R (N) := From_Hex (S (P .. P + 1));
         P     := P + 2;
      end loop;

      return R;
   end From_Hex;

   ------------------------------------------------------------------------
   -- To_Hex                                                             --
   ------------------------------------------------------------------------
   THex : constant array (0 .. 15) of Character := "0123456789ABCDEF";
   function To_Hex (C : Character) return Types.Hex_Character is
      use Interfaces;
      Byte : constant Unsigned_8 := Character'Pos (C);
   begin
      return "" &
         THex (Integer (Shift_right (Byte, 4))) &
         THex (Integer (Byte and 16#0f#));
   end To_Hex;

   function To_Hex (S : in String) return String is
      R : String (1 .. S'Length * 2);
      P : Natural := R'First;
   begin
      for N in S'Range loop
         R (P .. P + 1) := To_Hex (S (N));
         P              := P + 2;
      end loop;

      return R;
   end To_Hex;

   function To_Hex (I : in Natural; Length : in Natural := 0) return String is
      use Interfaces;
      Ui     : Unsigned_64 := Unsigned_64 (I);
      Result : String (1 .. 16);
      Pos    : Natural := Result'Last;
   begin
      while Ui /= 0 loop
         Result (Pos) := Thex (Integer (Ui and 16#0F#));
         Pos          := Pos - 1;
         Ui           := Shift_Right (Ui, 4);
      end loop;

      return Rpad (Result (Pos + 1 .. Result'Last), Length, '0');
   end To_Hex;

   function To_Hex (S : Ada.Streams.Stream_Element_Array) return String is
      Str : String (1 .. S'Length * 2);
      J   : Positive := Str'First;
   begin
      for I in S'Range loop
         Str (J .. J + 1) := To_Hex (Character'Val (S (I)));
         J := J + 2;
      end loop;
      return Str;
   end To_Hex;

   ---------------
   -- To_Stream --
   ---------------

   function To_Stream (S : String) return Ada.Streams.Stream_Element_Array is
      use Ada.Streams;
      Str : Stream_Element_Array (1 .. S'Length / 2);
      J   : Integer := S'First;
   begin
      for I in Str'Range loop
         Str (I) := Character'Pos (From_Hex (S (J .. J + 1)));
         J := J + 2;
      end loop;
      return Str;
   end To_Stream;

   ---------------
   -- To_String --
   ---------------

   function To_String (N : Integer) return String is
   begin
      return Trim (Integer'Image (N));
   end To_string;

   function To_String (N : Float; Decimals : Natural := 2) return String is
   begin
      return To_String (Long_Long_Float (N), Decimals);
   end To_string;

   function To_String (N : Long_Long_Float; Decimals : Natural := 2) return String is
      S : String (1 .. 8192); -- Maximum is E+4096; should fit here, right?
      P : Natural := 0;
   begin
      System.Img_Real.Set_Image_Real (N, S, P, 0, Decimals, 0);
      return S (1 .. P);
   end To_String;

--     function To_String (N : Long_Long_Float; Decimals : Natural := 2) return String is
--        type Prn is delta 0.000001 digits 16;
--     begin
--        if Decimals > 0 then
--           begin
--              declare
--                 Str : constant String := Trim (Prn'Image (Prn (N))) & "000000";
--              begin
--                 return
--                   Str (Str'First .. Pos (Str, ".") + Decimals);
--              end;
--           exception
--              when others =>
--                 --  Fallback in case the printer type can't hold N.
--                 return N'Img;
--           end;
--        else
--           return To_String (Integer (N));
--        end if;
--     end To_string;

   ------------
   -- To_Str --
   ------------

   function To_Str (N : Real; Decimals : Natural := 2) return String is
   begin
      if Decimals > 0 then
         return To_String (Long_Long_Float (N), Decimals);
      else
         return To_String (Integer (N));
      end if;
   end To_Str;

   ------------------
   -- Fixed_To_Str --
   ------------------

   function Fixed_To_Str (N        : Real;
                          Decimals : Natural := 2)
                          return     String
   is
      function S is new To_Str (Float);
   begin
      return S (Float (N), Decimals);
   end Fixed_To_Str;

   --------------------
   -- Decimal_To_Str --
   --------------------

   function Decimal_To_Str (N        : Real;
                            Decimals : Natural := 2)
                            return     String
   is
      Result : constant String := N'Img;
      use Strings.Fields;
   begin
      if Decimals > 0 then
         declare
            Pos : Natural := Result'First;
            Cnt : Natural := 0;
         begin
            while Result (Pos) /= '.' loop Pos := Pos + 1; end loop;
            while Pos < Result'Last loop
               Cnt := Cnt + 1; Pos := Pos + 1;
               exit when Cnt = Decimals;
            end loop;
            return Result (Result'First .. Pos);
         end;
      else
         return Select_Field (Result, 1, '.');
      end if;
   end Decimal_To_Str;

   type Str_Array is array (Positive range <>) of access constant String;

   Units : constant Str_Array  :=
             (new String'("B"),
              new String'("kB"),
              new String'("mB"),
              new String'("gB"),
              new String'("tB"));

   ------------------------------------------------------------------------
   -- To_size                                                            --
   ------------------------------------------------------------------------
   --  Beautifies some quantity (bytes) appending the correct units
   function To_size (
      Qty : in Integer; Decimals : in Natural := 2) return String
   is
      Pos   : Integer := Units'First;
      Q     : Float   := Float (Qty);
   begin
      if Qty < 0 then
         return "-" & To_size (-Qty);
      end if;

      loop
         exit when Q / 1024.0 < 1.0 or else Pos = Units'Last;
         Q   := Q / 1024.0;
         Pos := Pos + 1;
      end loop;

      return To_string (Q, Decimals) & " " & Units (Pos).all;
   end To_size;

   ------------------------------------------------------------------------
   -- To_size                                                            --
   ------------------------------------------------------------------------
   --  Beautifies some quantity (bytes) appending the correct units
   --  Long integer flavor
   function To_size (
      Qty : in Float; Decimals : in Natural := 2) return String
   is
      Pos   : Integer := Units'First;
      Q     : Float   := Qty;
   begin
      if Qty < 0.0 then
         return "-" & To_size (-Qty);
      end if;

      loop
         exit when Q / 1024.0 < 1.0 or else Pos = Units'Last;
         Q   := Q / 1024.0;
         Pos := Pos + 1;
      end loop;

      return To_string (Q, Decimals) & " " & Units (Pos).all;
   end To_size;


end Agpl.Conversions;
