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

with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Value_Function;

--  Common subprograms useable for all solvers.

package Agpl.Stochastics.Mdp.Solver.Common is

   pragma Preelaborate;

   procedure Appliable_Actions
     (States  : in     State.Object_Lists.List;
      A       : in     Action_Function;
      Actions :    out Containers.State_Actions_Maps.Map);
   --  Gives the actions appliable to some states.

   procedure Appliable_Outcomes
     (Initial  : in     State.Object'Class;
      A        : in     Action_Function;
      AE       : in     Action_Evolution_Function;
      Outcomes :    out Containers.Outcome_Lists.List);
   --  Gives the outcomes for a single state.

   procedure Appliable_Outcomes
     (States   : in     State.Object_Lists.List;
      A        : in     Action_Function;
      AE       : in     Action_Evolution_Function;
      Outcomes :    out Containers.State_Outcomes_Maps.Map);
   --  Gives the outcomes for a series of States.

   procedure Reachable_States
     (Initial : in     State.Object_Lists.List;
      E       : in     Evolution_Function;
      Final   :    out State.Object_Lists.List);
   --  Will compute all states reachable from Initial, at any distance.

   procedure Reachable_With_Policy
     (Initial : in     State.Object_Lists.List;
      AE      : in     Action_Evolution_Function;
      V       : in     Value_Function.Object;
      Final   :    out State.Object_Lists.List);
   --  All states reachable from Initial given some politic, at any distance.

   ---------------
   -- Summaries --
   ---------------

   --  Functions who drop in stdout some info about a problem.

   procedure Summary_State_Action
     (Initial : in     State.Object_Lists.List;
      E       : in     Evolution_Function;
      A       : in     Action_Function);
   --  Prints, for every state, how many actions can be applied to it.

end Agpl.Stochastics.Mdp.Solver.Common;
