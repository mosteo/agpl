 

package Agpl.Base64 is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- To_Base64                                                          --
   ------------------------------------------------------------------------
   function To_Base64 (Source : in String) return String;

   ------------------------------------------------------------------------
   -- To_String                                                          --
   ------------------------------------------------------------------------
   function To_String (Base64_String : in String) return String;

end Agpl.Base64;
