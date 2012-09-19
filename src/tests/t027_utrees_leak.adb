with Ada.Finalization; use Ada.Finalization;
with Agpl.Containers.Unbounded_Trees;
with Agpl.Os_Utils;
with Agpl.Text_Io; use Agpl.Text_Io;
with Agpl.Trace; use Agpl.Trace;
with Gnat.Debug_Pools;

--  This file requires exposing privates of Trees and
--  enabling the debug pool in smart_access_limited

--  THERE'S NO LONGER A LEAK. THE LEAK WAS IN
--  AGPL.SMART_ACCESS_LIMITED!!

procedure T027_Utrees_Leak is
   type Sides is (L, C, R);

   type Int is new Controlled with record
      I : Integer;
   end record;

--     overriding
--     procedure Finalize (I : in out Int) is
--     begin
--        Put_Line ("Int" & I.I'Img & " finalized");
--     end Finalize;

   package Side_Trees is
     new Agpl.Containers.Unbounded_Trees (Int, Sides, "<");
   use Side_Trees;

begin
--     Gnat.Debug_Pools.Configure (Side_Trees.Pool,
--                                 Reset_Content_On_Free => True,
--                                 Minimum_To_Free => Gnat.Debug_Pools.Ssc'Last,
--                                 Maximum_Logically_Freed_Memory => 1);

   Agpl.Os_Utils.Mtrace;

   Put_Line ("Starting...");
   for I in 1 .. 10 loop
      declare
         T : Side_Trees.Tree;
      begin
         T.Root.Clear;
      end;

      Put_Line ("AFTER ITERATION" & I'Img);
--        Gnat.Debug_Pools.Print_Info_Stdout (Side_Trees.Pool);
   end loop;

   Agpl.Os_Utils.Muntrace;

   Put_Line ("Done.");

--     Gnat.Debug_Pools.Print_Info_Stdout
--       (Side_Trees.Cursors.Pool,
--        Display_Leaks => True);

exception
   when E : others =>
      Put_Line (Report (E));
      Log ("Main: " & Report (E), Error);
end T027_Utrees_Leak;
