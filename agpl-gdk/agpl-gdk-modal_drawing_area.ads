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

with Gdk.Drawable; use Gdk.Drawable;
with Gdk;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Window;       use Gtk.Window;

with Ada.Finalization;

package Agpl.Gdk.Modal_Drawing_Area is

   --  pragma Elaborate_Body;

   type Object is tagged limited private;

   procedure Show (This  : in out Object;
                   Draw  : access procedure (Drawable : in Gdk_Drawable);
                   Title : in     String := "");
   --  Will call Draw on "expose" event.
   --  Display anything that has been drawn.
   --  Will not return until the window is closed.

private

   type Object is new Ada.Finalization.Limited_Controlled with record
      Area   : Gtk_Drawing_Area;
      Window : Gtk_Window;
      Draw   : access procedure (Drawable : in Gdk_Drawable);
   end record;

   type Object_Access is access all Object'Class;

   procedure Initialize (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Gdk.Modal_Drawing_Area;
