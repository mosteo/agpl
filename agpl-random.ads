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

with Ada.Numerics.Float_Random;

--  Shortcuts to some common random utilities.
--  This package if *not* thread safe.

--  During elaboration, the internal random generator is reset. This makes
--  each program run not repeatable.

package Agpl.Random is

   --  pragma Elaborate_Body;

   subtype Uniformly_Distributed is
     Ada.Numerics.Float_Random.Uniformly_Distributed;

   subtype Open_Uniformly_Distributed is Float range 0.0 .. Float'Pred (1.0);

   function Open_Uniform return Open_Uniformly_Distributed;
   --  Quick obtention of a random point in [0.0 .. 1.0)

   function Uniform return Uniformly_Distributed;
   --  Quick obtention of a random point in [0.0 .. 1.0]

   function Get_Float (Min, Max : in Float) return Float;
   --  Get a float in [Min .. Max]

   generic
      type Discrete is (<>);
   function Uniform_Discrete return Discrete;
   --  Obtain a random value from a discrete type.

   function Get_Integer (Min, Max : in Integer) return Integer;
   --  Quick obtention of a random integer in [Min, Max]
   --  if Max < Min then Max is returned

   type Object is tagged limited private;
   --  Use this object to have repeteability.
   --  Unless reset, this object has a default initialization which will make
   --  all runs identic.

   function Open_Uniform (This : in Object) return Open_Uniformly_Distributed;

   procedure Reset (This : in out Object);
   --  This resets based in some clock value.

   procedure Reset (This : in out Object; Initiator : in Integer);
   --  This resets to a certain generator.

   function Uniform (This : in Object) return Uniformly_Distributed;

   function Get_Integer (This     : in Object;
                         Min, Max : in Integer) return Integer;
   --  if Max < Min then Max is returned

   generic
      type Discrete is (<>);
   function Discrete_Random (This : in Object) return Discrete;

   function Flip_Coin return Boolean;
   --  50% chance of true

private

   type Object is tagged limited record
      Gen : Ada.Numerics.Float_Random.Generator;
   end record;

end Agpl.Random;
