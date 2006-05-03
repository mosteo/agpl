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

with Agpl.Stochastics.Mdp.Problem;
with Agpl.Stochastics.Mdp.Value_Function;

--  This solver will enumerate reachable states.
--  Will too enumerate appliable actions to a state and use only these.
--  Will too enumerate reachable states from an action and use only these.
--  Value iteration from that point.

package Agpl.Stochastics.Mdp.Solver.Naive3 is

   procedure Solve
     (Pr : in     Problem.Object;
      E  : in     Evolution_Function;
      T  : in     Transition_Function;
      R  : in     Reward_Function;
      A  : in     Action_Function;
      AE : in     Action_Evolution_Function;
      V  : in out Value_Function.Object;
      It : in     Positive := Positive'Last);
   --  It: Maximum iterations to attempt.

end Agpl.Stochastics.Mdp.Solver.Naive3;
