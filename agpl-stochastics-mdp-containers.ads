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

--  Containers used elsewhere in the MDP packages.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.Outcome;
with Agpl.Stochastics.Mdp.State;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;

package Agpl.Stochastics.Mdp.Containers is

   package State_Actions_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (State.Object_Id,
      Action.Object_Lists.List,
      State.Hash,
      State."=",
      Action.Object_Lists."=");

   package Outcome_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Outcome.Object'Class, Outcome."=");

   package State_Outcomes_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (State.Object_Id,
      Outcome_Lists.List,
      State.Hash,
      State."=",
      Outcome_Lists."=");

end Agpl.Stochastics.Mdp.Containers;
