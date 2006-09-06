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

with Agpl.Htn;
with Agpl.Htn.Tasks.Primitive;

package Agpl.Cr.Tasks.Starting_Pose is

   pragma Preelaborate;

   type Object (<>) is new Htn.Tasks.Primitive.Object with private;
   --  This special task is used to simplify the manipulation of task lists
   --  so there's no special case for the first task of an agent.

   --  For it to work, each Agent'Class implementation should treat this task
   --  as of cost 0 if matches its name and its the first task, infinite otherwise.
   --  Conversely, the cost from this task to any other is the cost from the agent
   --  current position to the task.

   --  As a variation, instead of reporting the cost from current pose to the task,
   --  it should report the cost from the last finished task to the next one.
   --  In this way, partially completed task lists can be evaluated using real
   --  posteriori costs plus estimated costs, and, in an ideal world, everything
   --  should match and be the same.

   --  Or, even better, if it reports the cost of all past tasks plus the
   --  estimation to the next, then no jumps in cost will occur.

   function Create (For_Agent : in String) return Object;

   function Get_Name (This : in Object) return String;

private

   type Object (Name_Len : Natural) is new Htn.Tasks.Primitive.Object with record
      Agent_Name : String (1 .. Name_Len);
   end record;

end Agpl.Cr.Tasks.Starting_Pose;
