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

package body Agpl.Gdk.Conversions is

   --------------
   -- To_Float --
   --------------

   function To_Float (N : Float) return Float is
   begin
      return N;
   end To_Float;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (N : Integer) return Gint is
   begin
      return Gint (N);
   end To_Gint;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (N : Float) return Gint is
   begin
      return Gint (N);
   end To_Gint;

end Agpl.Gdk.Conversions;
