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

--  Used to show some painting, until closed (not for interactive display).
--  The execution is not stopped; that is, a new window is opened in bg.

with Gdk.Drawable; use Gdk.Drawable;
with Gdk;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;

with Ada.Finalization;

package Agpl.Gdk.Managed.Drawing_Area is

   type Draw_Code is abstract new Gtk_Code with private;
   --  This type is used by the user to supply the drawing code in its Execute.

   function Drawable (This : in Draw_Code) return Gdk_Drawable;
   --  The user must use this in Execute to get the drawable.

   function Widget (This : in Draw_Code) return Gtk_Widget;
   --  The user must use this in Execute to get the widget containing the drawable.
   --  For example to get pango layouts.

   procedure Show (Draw  : in     Draw_Code'Class;
                   Title : in     String := "");
   --  A copy of Draw will be kept, so there shouldn't be references to external
   --  data that may have dissapeared.

private

   type Draw_Code is abstract new Gtk_Code with record
      Drawable : Gdk_Drawable;
      Widget   : Gtk_Widget;
   end record;

   type Access_Code is access all Draw_Code'Class;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Area   : Gtk_Drawing_Area;
      Window : Gtk_Window;
      Draw   : Access_Code;
   end record;

   type Object_Access is access all Object'Class;

   procedure Initialize (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Gdk.Managed.Drawing_Area;
