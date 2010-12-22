--  Proxy for drawing in a GdkDrawable, adding capabilities like autoscale,
--  zoom, axis flipping...

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Gdk.Drawable; use Gdk.Drawable;

package Agpl.Gdk.Drawer_Action is

   type Object is abstract tagged null record;
   --  This type is used to encapsulate drawing primitives, so anyone can
   --  provide new ones.

   procedure Prepare  (This   : in out Object;
                       Transf : in     Float_Matrix;
                       Min_X,
                       Max_X,
                       Min_Y,
                       Max_Y  :    out Float) is abstract;
   --  Pre-transform the figure and say enclosing box.

   procedure Perform (This   : in out Object;
                      Transf : in     Float_Matrix; -- Scaling transformation.
                      Draw   :        Gdk_Drawable) is abstract;
   --  Do the real drawing.

   package Action_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Drawer_Action.Object'Class, Drawer_Action."=");

   subtype Action_List is Action_Lists.List;

end Agpl.Gdk.Drawer_Action;
