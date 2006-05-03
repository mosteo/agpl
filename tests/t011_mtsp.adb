with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Optimization.Concorde;
use  Agpl.Optimization.Concorde;

procedure T011_mtsp is
   C : constant Cost_Matrix :=
         (1 => (0, 8, 1),
          2 => (8, 0, 9),
          3 => (1, 9, 0)
         );
   S : constant Start_Matrix :=
         (1,
          2
         );
begin
   declare
      Sol : constant Result_Matrix := Solve_MTSP (S, C);
   begin
      Print_Solution (C, Sol);
   end;
exception
   when No_Solution =>
      Put_Line ("NO VALID SOLUTION FOUND");
      raise;
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T011_mtsp;
