with Agpl.Xml;
pragma Elaborate_All (Agpl.Xml);

package Agpl.Xml.Utils is

   function Get_Attribute is
     new Get_Generic_Attribute (Boolean, Boolean'Image, Boolean'Value);

   function Get_Attribute is
     new Get_Generic_Attribute (Float, Float'Image, Float'Value);

   function Get_Attribute is
     new Get_Generic_Attribute (Integer, Integer'Image, Integer'Value);

end Agpl.Xml.Utils;
