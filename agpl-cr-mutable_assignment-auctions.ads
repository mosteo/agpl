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

with Agpl.Cr.Mutable_Assignment.Moves;

package Agpl.Cr.Mutable_Assignment.Auctions is

   pragma Elaborate_Body;

   --  use UNDO_MOVE_TASK for all moves in this package

   --  O (log)
   procedure Do_Auction_Task (This : in out Object;
                              Undo :    out Undo_Info);
   --  As undo, use the Undo_Move_Task
   --  Cost is kept logaritmic checking only a log fraction of all insertion points.

   procedure Do_Guided_Auction_Task (This : in out Object;
                                     Undo :    out Undo_Info);
   --  Guided in originating agent
   --  As undo, use the Undo_Move_Task

   --  O (n)
   procedure Do_Exhaustive_Auction_Task (This : in out Object;
                                         Undo :    out Undo_Info);
   --  As undo, use the Undo_Move_Task
   --  Will try all possible insertions

   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info)
     renames Moves.Undo_Move_Task;
   --  Will un-move all movements, in the Undo_Info stack, not just one.

end Agpl.Cr.Mutable_Assignment.Auctions;
