package body Agpl.Gdk.Widget_Bundle is

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (This : in out Object;
      W : access Gtk_Widget_Record'Class)
   is
   begin
      This.Root := Gtk_Widget (W);
   end Set_Root;

   --------------
   -- Get_Root --
   --------------

   function Get_Root (This : Object) return access Gtk_Widget_Record'Class is
   begin
      return This.Root;
   end Get_Root;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Object; Name : String;
                  W    : access Gtk_Widget_Record'Class) is
   begin
      This.Widgets.Include (Name, Gtk_Widget (W));
   end Set;

   ---------
   -- Get --
   ---------

   function Get (This : Object; Name : String) return access Gtk_Widget_Record'Class is
   begin
      return This.Widgets.Element (Name);
   end Get;

end Agpl.Gdk.Widget_Bundle;
