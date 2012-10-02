
--  Controlled version of a pango layout

with Pango.Layout;

private with Ada.Finalization;

package Agpl.Gdk.Pango_Layout is

   --  pragma Elaborate_Body;

   type Object (Layout : Pango.Layout.Pango_Layout) is tagged limited private;

private

   type Object (Layout : Pango.Layout.Pango_Layout) is
     new Ada.Finalization.Limited_Controlled with null record;

   procedure Finalize (This : in out Object);

end Agpl.Gdk.Pango_Layout;
