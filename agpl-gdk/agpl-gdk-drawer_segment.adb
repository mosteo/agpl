------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with Glib;

with Ada.Text_Io; use Ada.Text_Io;

package body Agpl.Gdk.Drawer_Segment is

   ------------
   -- Create --
   ------------

   function Create (Gc : Gdk_Gc; X1, Y1, X2, Y2 : Float) return Object is
   begin
      return (Gc => Gc,
              P1 => (X1, Y1, 1.0),
              P2 => (X2, Y2, 1.0));
   end Create;

   -------------
   -- Prepare --
   -------------

   procedure Prepare
     (This   : in out Object;
      Transf : in     Float_Matrix;
      Min_X,
      Max_X,
      Min_Y,
      Max_Y  :    out Float)
   is
   begin
      This.P1 := Transf * This.P1;
      This.P2 := Transf * This.P2;
      Min_X := Float'Min (This.P1 (1), This.P2 (1));
      Min_Y := Float'Min (This.P1 (2), This.P2 (2));
      Max_X := Float'Max (This.P1 (1), This.P2 (1));
      Max_Y := Float'Max (This.P1 (2), This.P2 (2));
   end Prepare;

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This   : in out Object;
      Transf : in     Float_Matrix;
      Draw   :        Gdk_Drawable)
   is
      T1 : constant Float_Vector := Transf * This.P1;
      T2 : constant Float_Vector := Transf * This.P2;
      use Glib;
   begin
      Draw_Line (Draw, This.Gc,
                 Gint (T1 (1)), Gint (T1 (2)),
                 Gint (T2 (1)), Gint (T2 (2)));
   exception
      when others =>
         Put_Line ("OBERFLOW: " &
                   This.P1 (1)'Img & This.P1 (2)'Img &
                   T1 (1)'Img & T1 (2)'Img);
   end Perform;

end Agpl.Gdk.Drawer_Segment;
