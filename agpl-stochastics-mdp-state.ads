------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
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
--  $Id: agpl.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

--  Root abstract type holding a State with the operations that will be needed
--  by the MDP Solver.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

package Agpl.Stochastics.Mdp.State is

   type Object is abstract tagged null record;

   type Object_Id is new String;

   function Distance (This : in Object) return Distances;
   --  Should provide a "distance" to the final state.
   --  Default one gives always 0.0 (wrong). You should redefine it if
   --  appliable for use elsewhere, don't care if not.

   function "<" (L, R : in Object'Class) return Boolean;
   --  Uses the distance and then the Id, to ensure that not < => >.

   function Get_Id (This : in Object) return Object_Id is abstract;
   --  Needed to conveniently store states and reference them.

   function Hash (Id : in Object_Id) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);
   --  Just a wrapper

   function To_String (This : in Object) return String;
   --  For debugging purposes

   --  Containers to be used elsewhere:
   package Object_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class, "=");

   package Object_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Object_Id, Object'Class);

   package Object_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Object'Class, "<");

   package Object_Id_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Object_Id, "<");

   package Object_Id_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Object_Id, "=");

end Agpl.Stochastics.Mdp.State;
