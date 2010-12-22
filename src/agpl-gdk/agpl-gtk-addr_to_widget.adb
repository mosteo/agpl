function Agpl.Gtk.Addr_To_Widget (Addr : System.Address) return Target is
   use Glib.Object;
   Dummy : Stub;
begin
   return Target (Get_User_Data (Addr, Dummy));
end Agpl.Gtk.Addr_To_Widget;
