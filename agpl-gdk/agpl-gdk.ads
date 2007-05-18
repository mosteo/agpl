package Agpl.Gdk is

   pragma Pure;

   type Float_Matrix is array (1 .. 3, 1 .. 3) of Float;
   type Float_Vector is array (1 .. 3) of Float;

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
   --  Dot product

   function "**" (L, R : Float_Vector) return Float_Vector;
   --  Cross product

   function H (X : Float_Vector) return Float_Vector;
   --  Homogeneization (i.e. X (3) = 1.0).

   function Inverse (X : Float_Matrix) return Float_Matrix;

private

   pragma Inline ("*");

end Agpl.Gdk;
