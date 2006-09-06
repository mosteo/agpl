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

with Agpl.Cr.Agent.Handle;
with Agpl.Cr.Assignment;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Containers;
with Agpl.Htn.Tasks.Handle;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Assigner.Greedy_Exhaustive is

   use type Agent.Containers.Lists.Cursor;
   use type Htn.Tasks.Containers.Lists.Cursor;
   use type Htn.Tasks.Task_Id;
   use type Cr.Agent.Handle.Object;
   use type Htn.Tasks.Handle.Object;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return Assignment.Object
   is
      A : Assignment.Object;
      --  The result we'll return.

      Pending : Htn.Tasks.Containers.Lists.List := Tasks;
      --  Tasks not yet assigned.

      -------------------------
      -- Remove_From_Pending --
      -------------------------

      procedure Remove_From_Pending (Id : in Htn.Tasks.Task_Id) is
         use Htn.Tasks.Containers.Lists;
         I : Cursor := Pending.First;
      begin
         while Has_Element (I) loop
            if Element (I).Get_Id = Id then
               Pending.Delete (I);
               return;
            else
               Next (I);
            end if;
         end loop;
         raise Program_Error; -- Shouldn't be reached.
      end Remove_From_Pending;

   begin
      --  Set agents
      declare
         use Cr.Agent.Containers.Lists;
         procedure Add (I : Cursor) is
         begin
            A.Set_Agent (Element (I));
         end Add;
      begin
         Agents.Iterate (Add'Access);
      end;

      --  Assign tasks:
      while not Pending.Is_Empty loop
         Log ("Pending:" & Pending.Length'Img, Always);

         declare
            New_Ass : Cr.Assignment.Object;
            Id_Used : Htn.Tasks.Task_Id;
            use Htn.Tasks;
         begin
            --  Insert best task in best agent:
            Cr.Tasks.Insertions.Greedy (A,
                    Pending,
                    Costs,
                    This.Criterion,
                    New_Ass,
                    Id_Used);
            if Id_Used /= No_Task then
               A := New_Ass;
               Remove_From_Pending (Id_Used);
            else
               A.Set_Valid (False);
               return A;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Agpl.Cr.Assigner.Greedy_Exhaustive;
