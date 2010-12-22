with Agpl.Gtk.Addr_To_Widget;
with Gtk.Button;
with Gtk.Table;
with Gtk.Toggle_Button;
with System;

package Agpl.Gtk.Widget_Factory is

   use Standard.Gtk;

   subtype Gobject_Ptr is System.Address;

   function Proxy is new Addr_To_Widget (Button.Gtk_Button_Record,
                                         Button.Gtk_Button);

   function Proxy is new Addr_To_Widget (Table.Gtk_Table_Record,
                                         Table.Gtk_Table);

   function Proxy is new Addr_To_Widget (Toggle_Button.Gtk_Toggle_Button_Record,
                                         Toggle_Button.Gtk_Toggle_Button);

end Agpl.Gtk.Widget_Factory;
