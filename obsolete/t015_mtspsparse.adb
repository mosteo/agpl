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

--  MTSP solutions for a given number of cities and travellers:
--  Invoke with: <program> <cities> <travelers> [random seed]

procedure T015_mtspsparse is

   package Colors renames Agpl.Gdk.Constants.Colors;
   package MDA renames Agpl.Gdk.Modal_Drawing_Area;

   procedure Usage is
   begin
      Put_Line ("Usage: <program> <cities> <travelers> [random seed]");
   end Usage;

   N : Cities;
   M : Salesmen;

   R : Generator;

   type String_Access is access all String;

   Color_List : constant array (1 .. 100) of String_Access :=
                  (1 => new String'("red"),
                   2 => new String'("#00aa00"),
                   3 => new String'("#0000ff"),
                   4 => new String'("magenta"),
                   others => new String'("cyan"));

begin
   if Argument_Count < 2 then
      Usage;
      return;
   end if;

   if Argument_Count = 2 then
      Reset (R);
   else
      Reset (R, Integer'Value (Argument (3)));
   end if;

   N := Cities'Value (Argument (1));
   M := Salesmen'Value (Argument (2));

   declare
      type City_Pos is record
         X, Y : Float;
      end record;

      City_Poses : array (1 .. N) of City_Pos;
      C : Cost_Matrix (1 .. N, 1 .. N);
      S : Start_Matrix (1 .. M);

      Starts : constant array (1 .. 4) of City_Pos :=
                 (1 => (0.0, 0.0),
                  2 => (0.0, 100.0),
                  3 => (100.0, 100.0),
                  4 => (100.0, 0.0));

   begin
      --  Fill random cities positions in coords <1 .. 100>
      for I in City_Poses'Range loop
         City_Poses (I).X := Random (R) * 100.0;
         City_Poses (I).Y := Random (R) * 100.0;
      end loop;

      --  Assign random starting places without repetition
      for I in S'Range loop
         declare
            Repeat : Boolean := False;
         begin
            loop
               S (I) := Cities (Random (R) * Float (N - 1) + 1.0);
               Repeat := False;
               for J in S'First .. I - 1 loop
                  if S (I) = S (J) then
                     Repeat := True;
                  end if;
               end loop;
               exit when not Repeat;
            end loop;
            --  Zero return to start city:
            --              for J in C'Range loop
            --                 C (J, S (I)) := 0;
            --              end loop;
         end;
      end loop;

      --  Force starting cities positions:
      for I in S'Range loop
         City_Poses (S (I)) := Starts (Integer (I));
      end loop;

      --  Compute distances
      for Row in C'Range loop
         for Col in C'Range (2) loop

            C (Row, Col) := Costs (Sqrt ((City_Poses (Row).X - City_Poses (Col).X) ** 2 +
                                           (City_Poses (Row).Y - City_Poses (Col).Y) ** 2));
         end loop;
      end loop;

      --  Solve!
      declare
         Sol : constant Result_Matrix := Solve_MTSP (S, C);
      begin
         print_Problem (C);
         for I in S'Range loop
            Put_Line ("Salesman" & I'Img & " starts at city" & S (I)'Img);
         end loop;
         Print_Solution (C, Sol);

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

               for I in S'Range loop
                  Drawer.Draw (Draw, Drawer_Arc.Create_Circle
                                 (Get_Color (Pal, Color_List (Integer (I)).all),
                                  City_Poses (S (I)).X,
                                  City_Poses (S (I)).Y,
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
end T015_mtspsparse;
