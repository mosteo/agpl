with Agpl.Cr.Map;
with Agpl.Transf2D;

package Test_Pkg is

   package Transf2DFloat is new Agpl.Transf2D (Float);

   type Int_Cell is new Agpl.Cr.Map.Observations with
      record
         I : Integer;
      end record;

end Test_Pkg;
