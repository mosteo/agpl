--  Simple test for the drawing area without and with update

with Ada.Calendar;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Agpl; use Agpl;
with Agpl.Drawing;
with Agpl.Gdk.Managed.Drawing_Area;
with Agpl.Trace;  use Agpl.Trace;

procedure T028_Drawing_Area is

   type Cross is new Drawing.Drawable with null record;
   overriding
   procedure Draw (This :        Cross;
                   D    : in out Drawing.Drawer'Class);

   procedure Draw (This :        Cross;
                   D    : in out Drawing.Drawer'Class)
   is
      pragma Unreferenced (This);
   begin
      D.Set_Color ((255, 0, 0), 0);
      D.Draw_Line (0.0, 0.0, 1.0, 1.0);
      D.Draw_Line (0.0, 1.0, 1.0, 0.0);
   end Draw;

   type Second is new Drawing.Drawable with null record;
   overriding
   procedure Draw (This :        Second;
                   D    : in out Drawing.Drawer'Class);

   procedure Draw (This :        Second;
                   D    : in out Drawing.Drawer'Class)
   is
      pragma Unreferenced (This);
      use Ada.Calendar;
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      Now    : constant Float := Float (Seconds (Clock));
      Offset : constant Float := Now - Float'Floor (Now);

   begin
      D.Set_Color ((0, 255, 0), 0);
      D.Draw_Line (0.0, 0.0, Sin (Offset * 2.0 * Pi), Cos (Offset * 2.0 * Pi));
   end Draw;

   X : Cross;
   Y : Second;
   Z : Second;

   Zh : Gdk.Managed.Drawing_Area.Handle :=
     Gdk.Managed.Drawing_Area.Show (Z, "Z");

begin
--     Set_Level (Debug);
--     Enable_Section (Agpl.Gdk.Managed.Log_Section);
--     Enable_Section (Agpl.Gdk.Managed.Det_Section);

   Gdk.Managed.Drawing_Area.Show (X, "X");
   Gdk.Managed.Drawing_Area.Show (Y, "Y");

   loop
      delay 0.01;
      Zh.Clear;
      Zh.Append (Z);
      Zh.Redraw;
   end loop;
exception
   when E : others =>
      Put_Line ("Main: " & Report (E));
end T028_Drawing_Area;
