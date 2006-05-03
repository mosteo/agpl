package body Agpl.Gdk.Constants is

   -----------
   -- Black --
   -----------

   function Black return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, 0, 0, 0);
      return C;
   end Black;

   ----------
   -- Blue --
   ----------

   function Blue return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, 0, 0, FFFF);
      return C;
   end Blue;

   ----------
   -- Gray --
   ----------

   function Gray return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, FF, FF, FF);
      return C;
   end Gray;

   -----------
   -- Green --
   -----------

   function Green  return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, 0, FFFF, 0);
      return C;
   end Green;

   ---------
   -- Red --
   ---------

   function Red return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, FFFF, 0, 0);
      return C;
   end Red;

   function Silver return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, 49152, 49152, 49152);
      return C;
   end Silver;

   -----------
   -- White --
   -----------

   function White return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, FFFF, FFFF, FFFF);
      return C;
   end White;

   -----------------
   -- Light_Green --
   -----------------

   function Light_Green return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, FF, FFFF, FF);
      return C;
   end Light_Green;

   ---------------
   -- Light_Red --
   ---------------

   function Light_Red   return Gdk_Color is
      C : Gdk_Color;
   begin
      Set_Rgb (C, FFFF, FF, FF);
      return C;
   end Light_Red;

end Agpl.Gdk.Constants;
