 

with Agpl.Conversions;
with Agpl.Strings;
with Agpl.Strings.Fields;

with Aws.Url;

with Ada.Exceptions;

package body Agpl.Magnet is

   use Attr_List;

   ------------------------------------------------------------------------
   -- AWS taken code/decode functions                                    --
   ------------------------------------------------------------------------
   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   --  Returns hexadecimal code for character C.
   function Code (C : in Character) return Escape_Code is
   begin
      return Conversions.To_Hex (C);
   end Code;

   -- As RFC mandated but without '.' and ':' encoding!
   Hex_Escape : constant array (Character) of Escape_Code
     := (';' => Code (';'), '/' => Code ('/'), '?' => Code ('?'),
         ' ' => Code (' '), '@' => Code ('@'), '&' => Code ('&'),
         '=' => Code ('='), '+' => Code ('+'), '$' => Code ('$'),
         ',' => Code (','), '<' => Code ('<'), '>' => Code ('>'),
         '#' => Code ('#'), '%' => Code ('%'), '"' => Code ('"'),
         '{' => Code ('{'), '}' => Code ('}'), '|' => Code ('|'),
         '\' => Code ('\'), '^' => Code ('^'), '[' => Code ('['),
         ']' => Code (']'), '`' => Code ('`'), others => Not_Escaped);

   ------------
   -- Decode --
   ------------
   function Decode (Str : in String) return String
      renames Aws.Url.Decode;

   ------------
   -- Encode --
   ------------
   function Encode (Str : in String) return String is
      Res : String (1 .. Str'Length * 3);
      K   : Natural := 0;
   begin
      for I in Str'Range loop
         if Hex_Escape (Str (I)) = Not_Escaped then
            K := K + 1;
            Res (K) := Str (I);
         else
            K := K + 1;
            Res (K) := '%';
            K := K + 1;
            Res (K .. K + 1) := Hex_Escape (Str (I));
            K := K + 1;
         end if;
      end loop;

      return Res (1 .. K);
   end Encode;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   function Create (Url : in String) return Object is
      This  : Object;
      Pos   : Natural := Strings.Pos (Url, "?"); -- Will point to previous &
      Fin   : Natural; -- Will point to each =
      Fin2  : Natural; -- Will point to next &
      Attr  : Attr_Record;
   begin
      This.Original_Url := U (Url);
      while Pos /= 0 loop
         Fin      := Strings.Pos (Url (Pos .. Url'Last), "=");
         if Fin = 0 then 
            Fin := Url'Last + 1;
         end if;
         Attr.Key := U (Url (Pos + 1 .. Fin - 1));
         Fin2     := Strings.Pos (Url (Fin + 1 .. Url'Last), "&");
         if Fin2 = 0 then 
            Fin2 := Url'Last + 1;
            Pos  := 0;
         else
            Pos := Fin2;
         end if;
         Attr.Value := U (Decode (Url (Fin + 1 .. Fin2 - 1)));
         Attr_List.Append (This.Attrs, Attr);
      end loop;

      return This;
   exception
      when others =>
         Ada.Exceptions.Raise_Exception (Invalid_Url'Identity, "Url provided: " & Url);
   end Create;

   ------------------------------------------------------------------------
   -- Add_Attribute                                                      --
   ------------------------------------------------------------------------
   -- Appends a new attribute value to the magnet
   procedure Add_Attribute (
      This : in out Object; Attr : in String; Value : in String)
   is
   begin
      Attr_List.Append (This.Attrs, (Key => U (Attr), Value => U (Value)));
   end Add_Attribute;

   ------------------------------------------------------------------------
   -- Get_Attribute                                                      --
   ------------------------------------------------------------------------
   -- Get any attribute of the magnet:
   function Get_Attribute (
      This : in Object; 
      Attr : in String   := ""; 
      Pos  : in Positive := 1) return String
   is
      C    : Cursor   := First (This.Attrs);
      N    : Positive := 1;
   begin
      while C /= No_Element loop
         if Attr = "" or else Attr = S (Element (C).Key) then
            if N = Pos then
               return S (Element (C).Value);
            else
               N := N + 1;
            end if;
         end if;
         Next (C);
      end loop;

      return "";
   end Get_Attribute;

   ------------------------------------------------------------------------
   -- Get_Hash_Type                                                      --
   ------------------------------------------------------------------------
   function Get_Hash_Type (This : in Object) return String is
   begin
      if Get_Num_Attributes (This, Uri_Attr) > 0 then
         return "several";
      else
         return 
            Strings.Fields.Select_Field (Get_Attribute (This, Uri_Attr), 2, ':');
      end if;
   end Get_Hash_Type;

   function Get_Hash_Type (This : in Object; Pos : in Positive) return String is
   begin
      if Get_Num_Attributes (This, Uri_Attr) < Pos then
         raise Constraint_Error;
      else
         return 
            Strings.Fields.Select_Field (
               Get_Attribute (This, Uri_Attr, Pos), 2, ':');
      end if;
   end Get_Hash_Type;

   ------------------------------------------------------------------------
   -- Get_Num_Attributes                                                 --
   ------------------------------------------------------------------------
   -- Of a given type [optionally]
   function Get_Num_Attributes (
      This : in Object;
      Attr : in String := "") return Natural
   is
      C    : Cursor   := First (This.Attrs);
      Num  : Natural  := 0;
   begin
      while C /= No_Element loop
         if Attr = "" or else Attr = S (Element (C).Key) then
            Num := Num + 1;
         end if;
         Next (C);
      end loop;
      return Num;
   end Get_Num_Attributes;

   ------------------------------------------------------------------------
   -- Get_Hash_Value                                                     --
   ------------------------------------------------------------------------
   function Get_Hash_Value (This : in Object; Kind : in String := "") 
      return String
   is
   begin
      if Kind = "" then
         return Strings.Fields.Select_Field (Get_Attribute (This, Uri_Attr), 3, ':');
      else
         for I in 1 .. Get_Num_Attributes (This, Uri_Attr) loop
            declare
               Hash : constant String := Get_Attribute (This, Uri_Attr, I);
            begin
               if Strings.Fields.Select_Field (Hash, 2, ':') = Kind then
                  return Strings.Fields.Select_Field (Hash, 3, ':');
               end if;
            end;
         end loop;
      end if;
      return "";
   end Get_Hash_Value;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (This : in Object) return String is
   begin
      return Get_Attribute (This, Name_Attr);
   end Get_Name;

   ------------------------------------------------------------------------
   -- Is_Magnet                                                          --
   ------------------------------------------------------------------------
   -- Says if a string is a Magnet link
   function Is_Magnet (This : in String) return Boolean is
   begin
      if This'Length >= 8 and then This (This'First .. This'First + 7) = "magnet:" then
         declare
            Dummy : constant Object := Create (This);
            pragma Unreferenced (Dummy);
         begin
            return true; -- Created OK!
         end;
      end if;

      return false;
   exception
      when others =>
         return false;
   end Is_Magnet;

   ------------------------------------------------------------------------
   -- To_String                                                          --
   ------------------------------------------------------------------------
   -- Returns the string form of the magnet
   function To_String (This : in Object) return String is
      use Attr_List;
      I   : Cursor  := First (This.Attrs);
      Url : Ustring := U ("magnet:?");
   begin
      loop
         ASU.Append (Url, Element (I).Key);
         ASU.Append (Url, "=");
         ASU.Append (Url, Encode (S (Element (I).Value)));
         Next (I);
         if I /= No_Element then
            ASU.Append (Url, "&");
         else
            exit;
         end if;
      end loop;
      return S (Url);
   end To_String;

end Agpl.Magnet;
