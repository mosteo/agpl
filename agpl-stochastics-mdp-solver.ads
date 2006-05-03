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

--  Root package for MDP solvers.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

package Agpl.Stochastics.Mdp.Solver is

   type Action_Function is access function
     (Initial : in State.Object'Class)
      return       Action.Object_Lists.List;
   --  This function must return all appliable actions to some state.

   type Action_Evolution_Function is access function
     (Initial : in State.Object'Class;
      Action  : in Mdp.Action.Object'Class)
      return       State.Object_Lists.List;
   --  All states reachable from a single state, given an action.

   type Evolution_Function is access function
     (Initial : in State.Object'Class)
      return       State.Object_Lists.List;
   --  This function must return all reachable states from another given one.

   type Involution_Function is access function
     (Final : in State.Object'Class)
      return     State.Object_Lists.List;
   --  Gives states from where the Final state is reachable.

   type Reward_Function is access function
     (Initial : in State.Object'Class;
      Doing   : in Action.Object'Class;
      Final   : in State.Object'Class)
      return       Rewards;
   --  This function is the typical reward function for MDPs

   type Transition_Function is access function
     (Initial : in State.Object'Class;
      Doing   : in Action.Object'Class;
      Final   : in State.Object'Class)
      return       Probabilities;
   --  This function is the typical transition function for MDPs

end Agpl.Stochastics.Mdp.Solver;
