with Smart_Pkg; use Smart_Pkg;

with Gnat.Debug_Pools;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_Io;    use Ada.Text_Io;

procedure Smartest is
   use Smart_Pkg.Smart;
begin
   Put_Line ("Start.");
   declare
--      P : Smart.Object := Bind (new Integer'(5));
--      Q : Smart.Object;
      I : Integer := 1;
   begin
--      Bind (Q, new Integer'(6));
      Gnat.Debug_Pools.Print_Info_Stdout (Smart_Pkg.Pool);
--      Put_Line (Integer'Image ( + P));
   end;

   Gnat.Debug_Pools.Print_Info_Stdout (Smart_Pkg.Pool);
   Put_Line ("End.");
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end Smartest;
