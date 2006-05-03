------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with Gdk.Event; use Gdk.Event;

with Gtk.Handlers;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main;
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
      This.Draw (Get_Window (This.Area));

      return False;
   end Expose;

   ----------
   -- Show --
   ----------

   procedure Show (This  : in out Object;
                   Draw  : access procedure (Drawable : in Gdk_Drawable);
                   Title : in     String := "")
   is
   begin
      This.Draw := Draw;
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
      Handlers_Ur.Connect
        (This.Window,
         "delete-event",
         Handlers_Ur.To_Marshaller (Destroy'Access),
         This'Unchecked_Access);

      Set_Position (This.Window, Win_Pos_Center);
      Set_Modal (This.Window, True);
      Realize (This.Window);

      Gtk_New (This.Area);

      --  Connect the expose event
      Handlers_Ur.Connect
        (This.Area,
         "expose-event",
         Handlers_Ur.To_Marshaller (Expose'Access),
         This'Unchecked_Access);

      Add (This.Window, This.Area);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      null;
   end Finalize;

end Agpl.Gdk.Modal_Drawing_Area;
