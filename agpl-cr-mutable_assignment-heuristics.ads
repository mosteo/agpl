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

package Agpl.Cr.Mutable_Assignment.Heuristics is

   pragma Elaborate_Body;

   procedure Undo_From_Scratch (This : in out Object; Undo : in Undo_Info)
     renames Mutable_Assignment.Undo_From_Scratch;
   --  Undo for all heuristics

   procedure Do_Heuristic_1 (This : in out Object;
                             Undo :    out Undo_Info)
     renames Mutable_Assignment.Do_Heuristic_1;
   --  Will consider all agents and tasks to provide some "good" assignment.
   --  The current tasks are re-assigned in a "best pair" greedy fashion.
   --  So no OR node switchings happen.

   procedure Do_Heuristic_2 (This : in out Object;
                             Undo :    out Undo_Info);
   --  This heuristic will consider the best of *all* tasks in every possible
   --  expansion; freeze the plan with the chosen task; repeat until no more T.

   --  O (n^2)
   procedure Do_Agent_Reorder (This : in out Object;
                               Undo :    out Undo_Info);
   --  Greedy reordering of an agent tasks

end Agpl.Cr.Mutable_Assignment.Heuristics;
