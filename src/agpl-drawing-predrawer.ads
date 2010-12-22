--  A helper drawable to perform some preliminary actions on a drawable.
--  E.g. change color

package Agpl.Drawing.Predrawer is

   pragma Preelaborate;

   type Prepare_Procedure is access procedure (Dest   : in out Drawer'Class;
                                               Target :        Drawable'Class);

   type Object (Prepare :                 Prepare_Procedure;
                Target  : access constant Drawable'Class)
     is new Drawable with null record;

   overriding
   procedure Draw (This :        Object;
                   D    : in out Drawer'Class);
   --  Will call Prepare (Target) and then Target.Draw

end Agpl.Drawing.Predrawer;
