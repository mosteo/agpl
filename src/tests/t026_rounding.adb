with Ada.Numerics;
with Agpl.Conversions;
with Agpl.Text_Io; use Agpl.Text_Io;

procedure T026_Rounding is
begin
   Put_Line ("Starting...");

   for I in 0 .. 10 loop
      Put_Line
        (Agpl.Conversions.To_String (Long_Long_Float'(Ada.Numerics.Pi), I));
   end loop;
   for I in 0 .. 10 loop
      Put_Line
        (Agpl.Conversions.To_String (Long_Long_Float'(-Ada.Numerics.Pi), I));
   end loop;

   Put_Line ("Done.");
end T026_Rounding;
