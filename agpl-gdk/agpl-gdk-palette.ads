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

--  This object is intended to encapsulate all needs for colors and GC in a
--  controlled way.

with Gdk.Color;    use Gdk.Color;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Finalization;

package Agpl.Gdk.Palette is

   --  pragma Elaborate_Body;

   type Object is tagged private;

   procedure Set_Drawable (This : in out Object; X : in Gdk_Drawable);
   --  Replaces the drawable in use. It will cause invalidation of all
   --  allocated resources in the former one.

   function Get_Color (This : access Object; Color : in String) return Gdk_Color;
   --  Allocates if necessary from default color map.
   --  Color must be a valid gdk parseable string (e.g. "#ffffff" for white).

   function Get_Color (This  : access Object;
                       Color : in String) return Gdk_GC;
   --  Allocates if necessary from default color map.
   --  Color must be a valid gdk parseable string (e.g. "#ffffff" for white).
   --  It gives an allocated GC instead of a color, ready for drawing.

   function Get_Gc (This       : access Object;
                    Color      : in     String;
                    Line_Width : in     Integer        := 1;
                    Line_Style : in     Gdk_Line_Style := Line_Solid)
                    return              Gdk_Gc;
   --  As previous, but with more options for line effects/fills

   function Get_Color (This : in Object; Color : in String) return Gdk_Color;
   function Get_Color (This : in Object; Color : in String) return Gdk_GC;
   --  As the two previous, but using 'Unrestricted_Access on This.
   --  So these two are GNAT specific.

   subtype Rgb_Component is Natural range 0 .. 255;

   function Get_Color_Name (R, G, B : Rgb_Component) return String;

private

   use type Gdk_Gc;

   package Color_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Gdk_Color);

   package Gc_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Gdk_Gc);

   type Object is new Ada.Finalization.Controlled with
      record
         Drawable : Gdk_Drawable;

         Colors   : Color_Maps.Map;
         Gcs      : Gc_Maps.Map;
      end record;

   procedure Initialize (This : in out Object);
   procedure Adjust   (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Gdk.Palette;
