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

--  An assigner creates assignments. Ideally it should aim to achieve some kind
--  of optimality.

with Agpl.Cr.Agent.Lists;
with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Matrix;
with Agpl.Htn.Plan;
with Agpl.Htn.Tasks.Lists;

package Agpl.Cr.Assigner is

   pragma Preelaborate;

   type Object is abstract tagged null record;

--     function Assign
--       (This   : in Object;
--        Agents : in Agent.Lists.List;
--        Tasks  : in Agpl.Htn.Tasks.Lists.List)
--        return      Assignment.Object is abstract;
   --  Takes a bunch of agents and tasks and says who must perform each task.
--  DEPRECATED. To avoid multiple calculation of costs.

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Agpl.Htn.Tasks.Lists.List;
      Costs  : in Cost_Matrix.Object)
      return      Assignment.Object is abstract;

   procedure Assign_Best_Plan
     (The_Assigner   : in     Object'Class;
      Agents         : in     Agent.Lists.List;
      Plans          : in     Htn.Plan.Object; -- OR of possible plans.
      Criterion      : in     Assignment_Criteria;
      Plan           :    out Htn.Plan.Object; -- Selected plan with best cost.
      The_Assignment :    out Assignment.Object);
   --  The plan maintains the methods and all that.
   --  No need to pass fake starting tasks, since this will be taken care of
   --  in the inside.

   --  DEBUG

   procedure Assign_Best_Plan
     (The_Assigner   : in     Object'Class;
      Agents         : in     Agent.Lists.List;
      Plans          : in     Htn.Plan.Object; -- OR of possible plans.
      Criterion      : in     Assignment_Criteria;
      Plan           :    out Htn.Plan.Object; -- Selected plan with best cost.
      The_Assignment :    out Assignment.Object;
      Enumerate      : access procedure (A : in Assignment.Object));
   --  As previous, but will call @Enumerate@ with each considered assignment

end Agpl.Cr.Assigner;
