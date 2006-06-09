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

--  Hierarchy starting here provides some utilities for displaying drawings
--  without having to take care of any Gdk/Gtk management. In other words,
--  a new thread for Gtk is created, and all Gtk related code is called from
--  within.

package Agpl.Gdk.Managed is

   pragma Elaborate_Body;

   type Gtk_Code is abstract tagged null record;
   --  This type must be extended to provide code to be executed.
   --  Note that the type is not limited. Any info required for drawing must
   --  be accessible at all times to the instance, since now the drawing is
   --  decoupled from other code flown.

   procedure Start;
   --  You must explicitely start the management, since this library can be
   --  included even if you don't plan to use it...

   procedure Execute (This : in out Gtk_Code) is abstract;
   --  Override this with the code required.

   procedure Shutdown;
   --  Call this to kill the Gtk thread.

private

   procedure Execute_In_Gtk (This : in out Gtk_Code'Class);
   --  Non dispatching. To be used by children of Managed to execute GTK code.

end Agpl.Gdk.Managed;
