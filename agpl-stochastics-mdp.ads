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

package Agpl.Stochastics.Mdp is

   pragma Preelaborate;

   Log_Section : constant String := "agpl.stochastics.mdp";

   --  Really, Markov matrix are square and its rows sum 1. This isn't
   --  enforceable in the type declaration so...
   type Markov_Matrix is
     array (Positive range <>, Positive range <>) of Probabilities;

   type Discounts is new Float range 0.0 .. 1.0;

   type Rewards is new Float;
   subtype Costs is Rewards;
   --  Costs and Rewards are equivalent, just the sign changes.

   type Reward_Array is array (Positive range <>) of Rewards;

   type Distances is new Float;

   Verbose : Boolean := False;
   --  If true, detailed progress messages will be printed by the solvers.

end Agpl.Stochastics.Mdp;
