with Ada.Numerics.Real_Arrays; use Ada.Numerics;

package Agpl.Gdk is

   pragma Preelaborate;

   Log_Section : constant String := "agpl.gdk";

   subtype Float_Matrix is Real_Arrays.Real_Matrix (1 .. 3, 1 .. 3);

   subtype Float_Vector is Real_Arrays.Real_Vector (1 .. 3);

   Identity : constant Float_Matrix :=
                ((1.0, 0.0, 0.0),
                 (0.0, 1.0, 0.0),
                 (0.0, 0.0, 1.0));
   Look_Up  : constant Float_Matrix :=
                ((0.0, -1.0, 0.0),
                 (1.0, 0.0, 0.0),
                 (0.0, 0.0, 1.0));
   --  To get easily a "from-behind" looking transform for things aligned
   --  along the X axis.

   function "*" (M : Float_Matrix; V : Float_Vector) return Float_Vector;
   pragma Inline ("*");
   --  Dot product

   function "**" (L, R : Float_Vector) return Float_Vector;
   pragma Inline ("**");
   --  Cross product

   function H (X : Float_Vector) return Float_Vector;
   --  Homogeneization (i.e. X (3) = 1.0).

   function Inverse (X : Float_Matrix) return Float_Matrix
     renames Real_Arrays.Inverse;

   --  Debug...
   procedure Print (X : Float_Matrix);

end Agpl.Gdk;
