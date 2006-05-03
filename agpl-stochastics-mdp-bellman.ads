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
with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Solver;
with Agpl.Stochastics.Mdp.State;
with Agpl.Stochastics.Mdp.Value_Function;

package Agpl.Stochastics.Mdp.Bellman is

   Wrong_Data : exception;
   --  Raised when the action probabilities don't add to 1.0

   type Result is private;
   --  This opaque type is used to return a Reward + Action pair.
   --  It's necessary because the Action is unconstrained.

   function Get_Action (This : in Result) return Action.Object'Class;
   pragma Inline (Get_Action);

   function Get_Reward (This : in Result) return Rewards;
   pragma Inline (Get_Reward);

   function Operator
     (Ini : in State.Object'Class;
      Act : in Action.Object_Lists.List;
      Fin : in State.Object_Lists.List;
      V   : in Value_Function.Object;
      T   : in Solver.Transition_Function;
      R   : in Solver.Reward_Function;
      D   : in Discounts) return Result;
   --  Applies the Bellman Operator, obtaining the new best Reward for a state.

   function Pruned_Operator
     (Ini : in State.Object'Class;
      O   : in Containers.Outcome_Lists.List;
      V   : in Value_Function.Object;
      T   : in Solver.Transition_Function;
      R   : in Solver.Reward_Function;
      D   : in Discounts) return Result;
   --  Applies the Bellman Operator, obtaining the new best Reward for a state.
   --  Uses an already pruned list of Outcomes for the State.

private

   type Result is record
      Reward : Rewards;
      Action : Mdp.Action.Object_Lists.List;
      --  A list containing a single action.
   end record;

end Agpl.Stochastics.Mdp.Bellman;
