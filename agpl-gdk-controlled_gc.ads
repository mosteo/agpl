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

--  A controlled GC to avoid resource leaks.
--  On creation it is allocated, on finalization it is unref'ed.

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;

with Ada.Finalization;

package Agpl.Gdk.Controlled_Gc is

   pragma Elaborate_Body;

   type Object (Draw : Gdk_Drawable) is tagged limited private;

   function Get_Gc (This : in Object) return Gdk_Gc;
   function "+" (This : in Object) return Gdk_Gc renames Get_Gc;

private

   type Object (Draw : Gdk_Drawable) is new Ada.Finalization.Limited_Controlled with
      record
         Gc : Gdk_Gc;
      end record;

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

end Agpl.Gdk.Controlled_Gc;
