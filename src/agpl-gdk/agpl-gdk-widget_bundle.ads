with Gtk.Widget; use Gtk.Widget;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Agpl.Gdk.Widget_Bundle is

   --  pragma Preelaborate;

   type Object is tagged private;

   procedure Set_Root (This : in out Object; W : access Gtk_Widget_Record'Class);
   function  Get_Root (This : Object) return access Gtk_Widget_Record'Class;

   procedure Set (This : in out Object; Name : String; W : access Gtk_Widget_Record'Class);
   function  Get (This : Object; Name : String) return access Gtk_Widget_Record'Class;

private

   package Widget_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Gtk_Widget);

   use Widget_Maps;

   type Object is tagged record
      Root    : Gtk_Widget;
      Widgets : Widget_Maps.Map;
   end record;

end Agpl.Gdk.Widget_Bundle;
