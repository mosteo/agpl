with Glib.Object;

package body Agpl.Gtk.User_Data is

   package Internal is new Glib.Object.User_Data (Data_Type);

   ----------------
   -- Set_Shared --
   ----------------

   procedure Set_Shared (Into : access Gtk_Widget_Record'Class;
                         Data : Data_Type;
                         Key  : String := Default_Key) is
   begin
      Internal.Set (Into.Get_Toplevel, Data, Key);
   end Set_Shared;

   ----------------
   -- Get_Shared --
   ----------------

   function Get_Shared (From : access Gtk_Widget_Record'Class;
                        Key  : String := Default_Key)
                        return Data_Type is
   begin
      return Internal.Get (From.Get_Toplevel, Key);
   end Get_Shared;

   ---------
   -- Set --
   ---------

   procedure Set (Into : access Gtk_Widget_Record'Class;
                  Data : Data_Type;
                  Key  : String := Default_Key)
   is
   begin
      Internal.Set (Into, Data, Key);
   end Set;

   ---------
   -- Get --
   ---------

   function  Get (From : access Gtk_Widget_Record'Class;
                  Key  : String := Default_Key)
                  return Data_Type
   is
   begin
      return Internal.Get (From, Key);
   end Get;

end Agpl.Gtk.User_Data;
