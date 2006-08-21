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

--  Abstract root for mapping classes.
--  We can have an underlying grid map, vectorial, etc.

package Agpl.Cr.Map is

   pragma Preelaborate;

   No_Data : exception;

   type Object is abstract tagged null record;
   --  The map itself.

   type Observations is abstract tagged null record;
   --  Data we want to store in it.

   function Get_At (This : in Object;
                    X, Y : in Float) return Observations'Class is abstract;
   --  Get data at some coords.
   --  May raise No_Data if nothing already there.

   function Is_Known (This : in Object;
                      X, Y : in Float) return Boolean is abstract;
   --  Returns true if there's any observation associated with the coordinates.

   procedure Set_At (This : in out Object;
                     X, Y : in     Float;
                     Obs  : in     Observations'Class) is abstract;
   --  Set/Merge information at given coords.

end Agpl.Cr.Map;
