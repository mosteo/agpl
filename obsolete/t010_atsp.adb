with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Optimization.Concorde;
use  Agpl.Optimization.Concorde;

procedure T010_atsp is
--     C : constant Cost_Matrix :=
--           (1 => (0, 1, 9, 4),
--            2 => (5, 0, 2, 1),
--            3 => (1, 2, 0, 3),
--            4 => (2, 1, 3, 0)
--           );
   C : constant Cost_Matrix := (
                                (0, 8, 1, 8, 0),
                                (8, 0, 9, 0, 8),
                                (1, 9, 0, 9, 1),
                                (0, 8, 1, Inf, Inf),
                                (8, 0, 9, Inf, Inf)
                                  );
begin
   declare
      Sol : constant Result_Matrix :=
              Solve_ATSP ((1 .. 1 => 3),
                          C);
   begin
      Print_Solution (C, Sol);
   end;
exception
   when No_Solution =>
      Put_Line ("NO VALID SOLUTION FOUND");
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T010_atsp;
