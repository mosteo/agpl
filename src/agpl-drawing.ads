with Agpl.Types;

package Agpl.Drawing is

   pragma Preelaborate;

   --  This type is the key abstraction to be used around...
   type Drawer is limited interface;

   type Drawer_Access is access all Drawer'Class;

   --  Primitives so any action can boil down to them and be reusable.
   procedure Draw_Line (This   : in out Drawer;
                        X1, Y1,
                        X2, Y2 : Float) is abstract;

   procedure Fill_Rectangle
     (This   : in out Drawer;
      X1, Y1,
      X2, Y2 : Float) is abstract;

   procedure Set_Color (This  : in out Drawer;
                        Rgb   :        Types.Rgb_Triplet;
                        Alpha :        Types.Unsigned_8) is abstract;

   procedure Write (This : in out Drawer;
                    X, Y : Float;
                    Utf8 : String) is abstract;

   --  Things that we want to show must implement this interface.
   --  It may also serve to encapsulate building drawing blocks.
   type Drawable is limited interface;
   type Drawable_Access is access all Drawable'Class;
   procedure Draw (This :        Drawable;
                   D    : in out Drawer'Class) is abstract;

   --  More drawing primitives that are compositions of the basics,
   --  sho they're classwide:

   procedure Draw_Rectangle (This   : in out Drawer'Class;
                             X1, Y1,
                             X2, Y2 : Float);

end Agpl.Drawing;
