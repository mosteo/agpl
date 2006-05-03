with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Optimization.Concorde;
use  Agpl.Optimization.Concorde;

--  Test for very small instances of TSP

procedure T017_tspsmall is
   No_Return : constant Boolean := True;
begin
   declare
      Start : constant Start_Matrix := (1 => 1);
      Costs : constant Cost_Matrix  := ((1, 2),
                                        (3, 4));
      Resul : constant Result_Matrix := Solve_MTSP (Start, Costs, No_Return);
   begin
      Print_Problem (Costs);
      Print_Solution (Costs, Start, Resul, No_Return);
   end;
exception
   when No_Solution =>
      Put_Line ("NO VALID SOLUTION FOUND");
      raise;
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T017_tspsmall;
