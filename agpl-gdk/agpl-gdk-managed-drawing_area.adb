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

with Ada.Unchecked_Deallocation;

package body Agpl.Gdk.Managed.Drawing_Area is

   package Handlers_UR is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Object_Access);

   --------------
   -- Drawable --
   --------------

   function Drawable (This : in Draw_Code) return Gdk_Drawable
   is
   begin
      return This.Drawable;
   end Drawable;

   ------------
   -- Widget --
   ------------

   function Widget (This : in Draw_Code) return Gtk_Widget is
   begin
      return This.Widget;
   end Widget;

   -------------
   -- Destroy --
   -------------

   function Destroy (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk_Event_Expose;
                     This   :        Object_Access)
                     return          Boolean
   is
      pragma Unreferenced (Widget, Event);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object'Class, Object_Access);

      Alias : Object_Access := This;
   begin
      Free (Alias); -- All allocated memory is gone now. Kwatz!

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
      This.Draw.Drawable := Get_Window (This.Area);
      This.Draw.Widget   := Gtk_Widget (This.Area);
      This.Draw.Execute; -- No need to do rendez-vous, we're already in the Gtk thread.

      return False;
   end Expose;

   ----------
   -- Show --
   ----------

   procedure Show (Draw  : in     Draw_Code'Class;
                   Title : in     String := "")
   is
      This : constant Object_Access := new Object;

      type Show_Code is new Gtk_Code with null record;
      procedure Execute (X : in out Show_Code) is
         pragma Unreferenced (X);
      begin
         Set_Title (This.Window, Title);
         Maximize (This.Window);
         Show_All (This.Window);
      end Execute;

   begin
      This.Draw := new Draw_Code'Class'(Draw);
      declare
         Show_It : Show_Code;
      begin
         Execute_In_Gtk (Show_It);
      end;
   end Show;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
      type Init_Code is new Gtk_Code with null record;
      procedure Execute (X : in out Init_Code) is
         pragma Unreferenced (X);
      begin
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
      end Execute;

      Do_It : Init_Code;
   begin
      Execute_In_Gtk (Do_It);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Draw_Code'Class, Access_Code);
   begin
      Free (This.Draw);
   end Finalize;

end Agpl.Gdk.Managed.Drawing_Area;
