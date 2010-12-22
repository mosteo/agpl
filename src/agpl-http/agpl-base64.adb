 

with Aws.Translator;

package body Agpl.Base64 is

   use Aws;

   ------------------------------------------------------------------------
   -- To_Base64                                                          --
   ------------------------------------------------------------------------
   function To_Base64 (Source : in String) return String is
   begin
      return Translator.Base64_Encode (Source);
   end To_Base64;

   ------------------------------------------------------------------------
   -- To_String                                                          --
   ------------------------------------------------------------------------
   function To_String (Base64_String : in String) return String is
   begin
      return Translator.To_String (Translator.Base64_Decode (Base64_String));
   end To_String;

end Agpl.Base64;
