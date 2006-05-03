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

--  grid implementation of a map.

with Ada.Containers.Indefinite_Ordered_Maps;

package Agpl.Cr.Map.Grid is

   pragma Preelaborate;

   type Object is new Map.Object with private;

   function Create (Resolution : in Float) return Object;
   --  No limits in size, just memory will tell.
   --  Resolution is the size (width x height) of the cells.

   function Get_At (This : in Object;
                    X, Y : in Float) return Observations'Class;
   --  Get data at some coords.

   function Is_Known (This : in Object;
                      X, Y : in Float) return Boolean;
   --  Returns true if there's any observation associated with the coordinates.

   procedure Set_At (This : in out Object;
                     X, Y : in     Float;
                     Obs  : in     Observations'Class);
   --  Set/Merge information at given coords.

private

   type Coordinates is record
      X, Y : Integer;
   end record;
   --  Used for indexing.

   function "<" (L, R : in Coordinates) return Boolean;

   package String_Obs_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Coordinates, Observations'Class);

   use String_Obs_Maps;

   --  The cells are stored in a map, using their coordinates as index.

   function Key (This : in Object; X, Y : in Float) return Coordinates;
   --  This will perform rounding of X, Y to the nearest grid point and provide
   --  the indexing key.
   --  Cells are always identified by their left-bottom coordinates.

   type Object is new Map.Object with
      record
         Cells : String_Obs_Maps.Map;
         Res   : Float := 0.0; -- Resolution
      end record;

end Agpl.Cr.Map.Grid;
