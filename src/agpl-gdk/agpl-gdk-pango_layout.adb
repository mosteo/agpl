
--  Controlled version of a pango layout

package body Agpl.Gdk.Pango_Layout is

   use Pango.Layout;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      Unref (This.Layout);
   end Finalize;

end Agpl.Gdk.Pango_Layout;
