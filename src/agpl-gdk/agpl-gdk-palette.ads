

--  This object is intended to encapsulate all needs for colors and GC in a
--  controlled way.

with Agpl.Types;

with Gdk.Color;    use Gdk.Color;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Finalization;

package Agpl.Gdk.Palette is

   --  pragma Elaborate_Body;

   type Object is tagged private;

   function Create (Gd : Gdk_Drawable) return Object;
   --  As Set_Drawable, for creation

   procedure Set_Drawable (This : in out Object; X : in Gdk_Drawable);
   --  Replaces the drawable in use. It will cause invalidation of all
   --  allocated resources in the former one.

   function Get_Color (This : access Object; Color : in String)
                       return Gdk_Color;
   pragma Precondition (Color /= "");
   --  Allocates if necessary from default color map.
   --  Color must be a valid gdk parseable string (e.g. "#ffffff" for white).

   function Get_Color (This  : access Object;
                       Color : in String) return Gdk_GC;
   pragma Precondition (Color /= "");
   --  Allocates if necessary from default color map.
   --  Color must be a valid gdk parseable string (e.g. "#ffffff" for white).
   --  It gives an allocated GC instead of a color, ready for drawing.

   function Get_Gc (This       : access Object;
                    Color      : in     String;
                    Line_Width : in     Integer        := 1;
                    Line_Style : in     Gdk_Line_Style := Line_Solid)
                    return              Gdk_GC;
   pragma Precondition (Color /= "");
   --  As previous, but with more options for line effects/fills

   function Get_Color (This : in Object; Color : in String) return Gdk_Color;
   pragma Precondition (Color /= "");

   function Get_Color (This : in Object; Color : in String) return Gdk_GC;
   pragma Precondition (Color /= "");
   --  As the two previous, but using 'Unrestricted_Access on This.
   --  So these two are GNAT specific.

   subtype Rgb_Component is Types.Unsigned_8;

   function Get_Color_Name (R, G, B : Rgb_Component) return String;

   function Get_Gradient (Val_In_Domain                : Float;
                          Min_In_Domain, Max_In_Domain : Float;
                          Min_In_Rgb,    Max_In_Rgb    : Rgb_Component)
                          return Rgb_Component;

   generic
      type Domain_range is digits <>;
      Rgb_Min : Rgb_Component;
      Rgb_Max : Rgb_Component; -- Min can be > Max, to reverse the gradient
   function Gradient (Val : Domain_Range'Base) return Rgb_Component;

private

   use type Gdk_GC;

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
