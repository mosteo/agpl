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

--  Used to pack an action with its possible outcomes.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

package Agpl.Stochastics.Mdp.Outcome is

   type Object is tagged private;

   function Create
     (A : in Action.Object'Class;
      S : in State.Object_Lists.List) return Object;
   --  Receives an action and all the valid reachable states for these.

   function Get_Action (This : in Object) return Action.Object'Class;

   function Get_Action (This : in Object) return Action.Object_Lists.List;
   --  Returns a list containing a single action.

   function Get_States (This : in Object) return State.Object_Lists.List;

private

   type Object is tagged record
      Action : Mdp.Action.Object_Lists.List;
      --  A list with a single element.

      States : State.Object_Lists.List;
      --  States
   end record;

end Agpl.Stochastics.Mdp.Outcome;
