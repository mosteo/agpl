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

package Agpl.Htn.Plan.Utils is

   pragma Preelaborate;

   Log_Section : constant String := "agpl.htn.plan.utils";

   procedure Replace_Child (This        : in out Plan.Object;
                            Parent_Node,
                            New_Child,
                            Old_Child   : in     Subplan);
   --  Replace the Old_Child (whose parent is Parent_Node) by a new one.
   --  The Old is freed! The New is deep copied (because the entire branch
   --  is freed) but no extra memory is left dangling.

   procedure Trim_OR_Siblings (This : in out Plan.Object;
                               Job  : in     Tasks.Task_Id);
   --  Given a plan and a task id, will replace an OR branch containing the
   --  Task by the task proper.
   --  This is useful for example to merge an assignment with a plan and ob-
   --  tain a plan compatible

end Agpl.Htn.Plan.Utils;
