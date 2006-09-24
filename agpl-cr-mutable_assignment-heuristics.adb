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

--  The difference with Expres.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Cr.Agent.Handle;
with Agpl.Cr.Plan_Assigner;
with Agpl.Cr.Plan_Assigner.Greedy1;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Trace;   use Agpl.Trace;

package body Agpl.Cr.Mutable_Assignment.Heuristics is

   --------------------
   -- Do_Heuristic_2 --
   --------------------

   procedure Do_Heuristic_2 (This : in out Object;
                             Undo :    out Undo_Info)
   is
      U : Undo_Internal (From_Scratch);
   begin
      U.Ass         := This.To_Assignment;
      U.Description := +"Heuristic 2";
      Undo.Handle.Set (U);

      declare
         use Cr.Assignment;
         New_Assignment : constant Cr.Assignment.Object :=
                            Plan_Assigner.Greedy1.Assign
                              ((Plan_Assigner.Object with null record),
                               Get_Agents_Without_Tasks (U.Ass),
                               This.Context.Ref.Plan,
                               This.Context.Ref.Costs.Ref.all,
                               This.Context.Ref.Criterion);
      begin
--         New_Assignment.Print_Assignment;

         if New_Assignment.Is_Valid then
            Set_Assignment (This, New_Assignment, This.Context.Ref.Criterion);
         else
            Log ("Plan_Assigner.Greedy1 failed!", Warning, Log_Section);
         end if;
         --  Note: here Minimax will not be used since there are no new tasks.
      end;
   exception
      when E : Constraint_Error =>
         Log ("Plan_Assigner.Greedy1 failed!", Warning, Log_Section);
         Log (Report (E), Warning);
   end Do_Heuristic_2;

   ----------------------
   -- Do_Agent_Reorder --
   ----------------------

   procedure Do_Agent_Reorder (This : in out Object;
                               Undo :    out Undo_Info)
   is
      U     : Undo_Internal (From_Scratch);
      Agent : constant Agent_Id :=
                Agent_Id
                  (+Agent_Context
                     (This.Select_Random_Context (All_Agents).all).Agent_Name);
   begin
      declare
         New_Ass : Assignment.Object := This.To_Assignment;
         Ag      : Cr.Agent.Object'Class := New_Ass.Get_Agent (String (Agent));
         Tasks   : Task_Lists.List := Ag.Get_Tasks;
      begin
         U.Description := +"AGENT REORDER N²";
         U.Ass         := New_Ass;
         Undo.Handle.Set (U);

         Ag.Clear_Tasks;
         while not Tasks.Is_Empty loop
            declare
               New_Ag : Cr.Agent.Handle.Object;
               Cd, Ct : Cr.Costs;
               Ok     : Boolean;
            begin
               Cr.Tasks.Insertions.Greedy (Ag,
                                           Tasks.First_Element,
                                           New_Ag,
                                           Cd, Ct, Ok);
               if not Ok then
                  Log ("Failed to reorder agent tasks", Warning, Log_Section);
                  This.Do_Identity (Undo);
                  return;
               else
                  Ag := New_Ag.Get;
                  Tasks.Delete_First;
               end if;
            end;
         end loop;
         New_Ass.Set_Agent (Ag);
         New_Ass.Set_Valid;
--         New_Ass.Print_Summary;
         This.Set_Assignment (New_Ass, This.Context.Ref.Criterion);
      end;
   end Do_Agent_Reorder;

end Agpl.Cr.Mutable_Assignment.Heuristics;
