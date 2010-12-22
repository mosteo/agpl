 

--  A controlled GC to avoid resource leaks.
--  On creation it is allocated, on finalization it is unref'ed.

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;

with Ada.Finalization;

package Agpl.Gdk.Controlled_Gc is

   pragma Elaborate_Body;

   type Object (Draw : Gdk_Drawable) is tagged limited private;

   function Get_Gc (This : in Object) return Gdk_Gc;
   function "+" (This : in Object) return Gdk_Gc renames Get_Gc;

private

   type Object (Draw : Gdk_Drawable) is new Ada.Finalization.Limited_Controlled with
      record
         Gc : Gdk_Gc;
      end record;

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

end Agpl.Gdk.Controlled_Gc;
