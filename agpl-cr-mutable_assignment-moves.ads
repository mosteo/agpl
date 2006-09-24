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

package Agpl.Cr.Mutable_Assignment.Moves is

   pragma Elaborate_Body;

   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info);
   --  Will un-move all movements, in the Undo_Info stack, not just one.

   --  O (log)
   procedure Do_Move_Task (This : in out Object;
                           Undo :    out Undo_Info);

   --  O (log)
   procedure Do_Move_Task_Changing_Owner (This : in out Object;
                                          Undo :    out Undo_Info);
   --  Moves a task at random, but choses the owner before hand. In this way,
   --  no agent can end without tasks (as happens when just using Move_Task
   --  As undo, use the Undo_Move_Task

   procedure Do_Guided_Move_Task_Changing_Owner (This : in out Object;
                                                 Undo :    out Undo_Info);
   --  Like previous, but task is chosen from the worst cost agent

   procedure Do_Swap_Order (This : in out Object;
                            Undo :    out Undo_Info);
   --  Switches two consecutive tasks
   --  As undo, use the Undo_Move_Task

   procedure Do_Swap_Tasks (This : in out Object;
                            Undo :    out Undo_Info);
   --  Switches two arbitrary tasks
   --  As undo, use the Undo_Move_Task

end Agpl.Cr.Mutable_Assignment.Moves;
