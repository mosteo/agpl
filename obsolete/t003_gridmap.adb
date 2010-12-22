with Test_Pkg;     use Test_Pkg;

with Agpl.Cr.Map.Grid; use Agpl.Cr.Map.Grid;
with Agpl.Strings; use Agpl.Strings;

with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

procedure T003_Gridmap is
   Map : Object := Create (0.4);
begin
   Set_At (Map, 0.0, 0.0, Int_Cell'(I => 1));
   Set_At (Map, 0.4, 0.4, Int_Cell'(I => 2));

   Put_Line ("Cell:" & Integer'Image (Int_Cell (Get_At (Map, 0.0, 0.0)).I));
   Put_Line ("Cell:" & Integer'Image (Int_Cell (Get_At (Map, 0.39, 0.39)).I));
   Put_Line ("Cell:" & Integer'Image (Int_Cell (Get_At (Map, 0.4, 0.4)).I));
   Put_Line ("Cell:" & Integer'Image (Int_Cell (Get_At (Map, -0.001, 0.0)).I));
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T003_Gridmap;
