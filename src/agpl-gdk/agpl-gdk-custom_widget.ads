with Agpl.Gdk.Widget_Bundle;
with Gtk.Widget;             use Gtk.Widget;

package Agpl.Gdk.Custom_Widget is

   --  pragma Preelaborate;

   type Remote is interface;
   --  The idea is that the type is sent across the network, and the widget
   --  is created locally and subsequently updated with each new reception.

   function Create (This : Remote) return Widget_Bundle.Object is abstract;

   procedure Update (This : in out Remote;
                     Guts : in out Widget_Bundle.Object) is abstract;

end Agpl.Gdk.Custom_Widget;
