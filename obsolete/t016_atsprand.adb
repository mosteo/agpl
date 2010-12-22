with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Gdk.Constants;
with Agpl.Gdk.Drawer;
with Agpl.Gdk.Drawer_Arc;
with Agpl.Gdk.Drawer_Figures;
with Agpl.Gdk.Drawer_Rectangle;
with Agpl.Gdk.Modal_Drawing_Area;
with Agpl.Gdk.Palette;

with Agpl.Optimization.Concorde;
use  Agpl.Optimization.Concorde;

with Gdk.Drawable; use Gdk.Drawable;

with Ada.Command_Line;
use  Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Gnat.Os_Lib;

--  ATSP solutions for a given number of cities:
--  Invoke with: <program> <cities> [seed]
--  Each city has random cost in either way.

procedure T016_atsprand is

   package Colors renames Agpl.Gdk.Constants.Colors;
   package MDA renames Agpl.Gdk.Modal_Drawing_Area;

   procedure Usage is
   begin
      Put_Line ("Usage: <program> <cities> [random seed]");
   end Usage;

   N : Cities;

   R : Generator;

   type String_Access is access all String;

   Color_List : constant array (1 .. 4) of String_Access :=
                  (1 => new String'("red"),
                   2 => new String'("#00aa00"),
                   3 => new String'("#0000ff"),
                   4 => new String'("magenta"));

begin
   if Argument_Count < 1 then
      Usage;
      return;
   end if;

   if Argument_Count = 1 then
      Reset (R);
   else
      Reset (R, Integer'Value (Argument (2)));
   end if;

   N := Cities'Value (Argument (1));

   declare
      type City_Pos is record
         X, Y : Float;
      end record;

      City_Poses : array (1 .. N) of City_Pos;
      C : Cost_Matrix (1 .. N, 1 .. N);

   begin
      --  Fill random cities positions in coords <1 .. 100>
      for I in City_Poses'Range loop
         City_Poses (I).X := Random (R) * 100.0;
         City_Poses (I).Y := Random (R) * 100.0;
      end loop;

      --  Compute distances completely at random
      for Row in C'Range loop
         for Col in C'Range (2) loop
            if Row = Col then
               C (Row, Col) := 0;
            else
               C (Row, Col) := Costs (Random (R) * 100.0) + 2;
            end if;
         end loop;
      end loop;

      declare
         Sol : constant Result_Matrix := Solve_ATSP ((1 => 1), C);
      begin
         print_Problem (C);
         Print_Solution (C, (1 => 1), Sol, No_Return => False);

         --  Display graphically the solution:
         declare
            Area : MDA.Object;

            -------------------
            -- Draw_Solution --
            -------------------

            procedure Draw_Solution (Drawable : in Gdk_Drawable)
            is
               use Agpl.Gdk;
               use Agpl.Gdk.Palette;
               Pal  : aliased Palette.Object;
               Draw : Drawer.Object;
            begin
               Pal.Set_Drawable (Drawable);
               Draw.Set_Drawable (Drawable);

               Drawer.Draw_Begin (Draw);

               --  Draw border
               Drawer.Draw (Draw, Drawer_Rectangle.Create
                              (Get_Color (Pal, Colors.White),
                               0.0, 0.0, 100.0, 100.0, Fill => True));
               Drawer_Figures.Draw_Segment (0.0, 0.0, 0.0, 100.0,
                                            Get_Color (Pal, Colors.Black),
                                            Draw);
               Drawer_Figures.Draw_Segment (0.0, 100.0, 100.0, 100.0,
                                            Get_Color (Pal, Colors.Black),
                                            Draw);
               Drawer_Figures.Draw_Segment (100.0, 100.0, 100.0, 0.0,
                                            Get_Color (Pal, Colors.Black),
                                            Draw);
               Drawer_Figures.Draw_Segment (100.0, 0.0, 0.0, 0.0,
                                            Get_Color (Pal, Colors.Black),
                                            Draw);

               --  Draw cities
               for I in City_Poses'Range loop
                  Drawer.Draw (Draw, Drawer_Arc.Create_Circle
                                 (Get_Color (Pal, "#aaaaaa"),
                                  City_Poses (I).X, City_Poses (I).Y, 2.0,
                                  Fill => True));
               end loop;

               for I in 1 .. 1 loop
                  Drawer.Draw (Draw, Drawer_Arc.Create_Circle
                                 (Get_Color (Pal, Color_List (Integer (I)).all),
                                  City_Poses (1).X,
                                  City_Poses (1).Y,
                                  2.0,
                                  Fill => True));
               end loop;

               --  Draw tours
               for Salesman in Sol'Range loop
                  for City in Sol'First (2) .. Sol'Last (2) - 1 loop
                     Drawer_Figures.Draw_Segment
                       (City_Poses (Sol (Salesman, City)).X,
                        City_Poses (Sol (Salesman, City)).Y,
                        City_Poses (Sol (Salesman, City + 1)).X,
                        City_Poses (Sol (Salesman, City + 1)).Y,
                        Get_Color (Pal, Color_List (Integer (Salesman)).all),
                        Draw);
                  end loop;
                  Drawer_Figures.Draw_Segment
                    (City_Poses (Sol (Salesman, Sol'Last (2))).X,
                     City_Poses (Sol (Salesman, Sol'Last (2))).Y,
                     City_Poses (Sol (Salesman, Sol'First (2))).X,
                     City_Poses (Sol (Salesman, Sol'First (2))).Y,
                     Get_Color (Pal, Colors.Black), -- Draw in black the returning line
                     Draw);
               end loop;

               --  Show
               Drawer.Draw_End (Draw);
            end Draw_Solution;

         begin
            Area.Show (Draw_Solution'Access);
         end;
      end;
   end;
exception
   when No_Solution =>
      Put_Line ("NO VALID SOLUTION FOUND");
      raise;
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T016_atsprand;
