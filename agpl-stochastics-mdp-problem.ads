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

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

package Agpl.Stochastics.Mdp.Problem is

   pragma Preelaborate;

   type Object is private;

   procedure Create
     (This            :    out Object;
      Initial_States  : in     State.Object_Lists.List;
      Final_States    : in     State.Object_Lists.List;
      Actions         : in     Action.Object_Lists.List;
      Discount        : in     Discounts := 0.95);
   --  Creates the basic definition of a problem

   function Get_Actions
     (This : in Object)
      return Action.Object_Lists.List;
   --  Obtain the actions applying to this MDP.

   function Get_Discount (This : in Object) return Discounts;

   function Get_Final_States (This : in Object)
                              return State.Object_Lists.List;
   --  Obtain desired final states (if apply).

   function Get_Initial_States (This : in Object)
                                return State.Object_Lists.List;
   --  Obtain the initial states for the problem

private

   type Object is record
      Initial_States : State.Object_Lists.List;
      Final_States   : State.Object_Lists.List;
      Actions        : Action.Object_Lists.List;
      Discount       : Discounts;
   end record;

end Agpl.Stochastics.Mdp.Problem;
