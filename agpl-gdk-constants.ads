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

with Gdk.Color; use Gdk.Color;

package Agpl.Gdk.Constants is

   pragma Elaborate_Body;

   FFFF : constant := 2 ** 16 - 1; -- 65535
   FF   : constant := 2 ** 8 - 1; -- 255

   function Black  return Gdk_Color;
   function Blue   return Gdk_Color;
   function Gray   return Gdk_Color;
   function Green  return Gdk_Color;
   function Red    return Gdk_Color;
   function Silver return Gdk_Color;
   function White  return Gdk_Color;

   function Light_Green return Gdk_Color;
   function Light_Red   return Gdk_Color;

   package Colors is
      Black : constant String := "#000000";
      Blue  : constant String := "#0000ff";
      White : constant String := "#ffffff";
   end Colors;

end Agpl.Gdk.Constants;
