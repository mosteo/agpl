with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Optimization.Concorde;
use  Agpl.Optimization.Concorde;

procedure T009_tsp is
   C : constant Cost_Matrix :=
         (1 => (0, 1, 2, 1),
          2 => (1, 0, 1, 2),
          3 => (2, 1, 0, 1),
          4 => (1, 2, 1, 0));
begin
   declare
      Sol : constant Result_Matrix :=
              Solve_TSP ((1 .. 1 => 3),
                         C);
   begin
      for Salesman in Sol'Range (1) loop
         Put_Line ("Salesman" & Salesman'Img);
         for City in Sol'Range (2) loop
            Put (Sol (Salesman, City)'Img);
         end loop;
         New_Line;

         Put_Line ("Total cost:   " & Get_Total_Cost (C, Sol)'Img);
         Put_Line ("Min-max cost: " & Get_Min_Max_Cost (C, Sol)'Img);
      end loop;
   end;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T009_tsp;
