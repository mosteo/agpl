with Gtk.Widget; use Gtk.Widget;

--  Storing of data into widgets

generic
   type Data_Type (<>) is private;
package Agpl.Gtk.User_Data is

   Default_Key : constant String := "user_data";

   procedure Set_Shared (Into : access Gtk_Widget_Record'Class;
                         Data : Data_Type;
                         Key  : String := Default_Key);
   --  It is stored at the root widget in this hierarchy.
   --  So beware of overlappings

   function  Get_Shared (From : access Gtk_Widget_Record'Class;
                         Key  : String := Default_Key)
                         return Data_Type;

   procedure Set (Into : access Gtk_Widget_Record'Class;
                  Data : Data_Type;
                  Key  : String := Default_Key);
   --  It is stored at the given widget

   function  Get (From : access Gtk_Widget_Record'Class;
                  Key  : String := Default_Key)
                  return Data_Type;

end Agpl.Gtk.User_Data;
