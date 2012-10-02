

with Gdk.Event; use Gdk.Event;

with Gtk.Handlers; pragma Elaborate_All (Gtk.Handlers);
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main;
--  with Gtk.Marshallers; pragma Elaborate_All (Gtk.Marshallers);
with Gtk.Widget; use Gtk.Widget;

package body Agpl.Gdk.Modal_Drawing_Area is

   package Handlers_UR is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Object_Access);

   -------------
   -- Destroy --
   -------------

   function Destroy (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk_Event_Expose;
                     This   :        Object_Access)
                     return          Boolean
   is
      pragma Unreferenced (Widget, Event, This);
   begin
      Gtk.Main.Main_Quit;

      return False;
   end Destroy;

   ------------
   -- Expose --
   ------------

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk_Event_Expose;
                    This   :        Object_Access)
                    return          Boolean
   is
      pragma Unreferenced (Widget, Event);
   begin
      This.Draw.Ref.Draw (Get_Window (This.Area));

      return False;
   end Expose;

   ----------
   -- Show --
   ----------

   procedure Show (This  : in out Object;
                   Draw  :        Callback'Class;
                   Title : in     String := "")
   is
   begin
      This.Draw.Set (Draw);
      Set_Title (This.Window, Title);
      Maximize (This.Window);
      Show_All (This.Window);
      Gtk.Main.Main;
   end Show;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
   begin
      Gtk.Main.Init;

      Gtk_New (This.Window);
      Initialize (This.Window, Window_Toplevel);

      --  Connect the destroy event
      Handlers_UR.Connect
        (This.Window,
         "delete-event",
         Handlers_UR.To_Marshaller (Destroy'Access),
         This'Unchecked_Access);

      Set_Position (This.Window, Win_Pos_Center);
      Set_Modal (This.Window, True);
      Realize (This.Window);

      Gtk_New (This.Area);

      --  Connect the expose event
      Handlers_UR.Connect
        (This.Area,
         "expose-event",
         Handlers_UR.To_Marshaller (Expose'Access),
         This'Unchecked_Access);

      Add (This.Window, This.Area);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
      pragma Unreferenced (This);
   begin
      null;
   end Finalize;

end Agpl.Gdk.Modal_Drawing_Area;
