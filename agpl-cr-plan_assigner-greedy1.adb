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

--  with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Htn.Plan_Node;
with Agpl.Htn.Plan.Utils;
with Agpl.Htn.Tasks.Containers;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Plan_Assigner.Greedy1 is

   use type Htn.Tasks.Task_Id;

   ------------
   -- Assign --
   ------------

   function Assign
     (This      : in Object;
      Agents    : in Agent.Containers.Vectors.Vector;
      Plan      : in Agpl.Htn.Plan.Object;
      Costs     : in Cost_Cache.Object'Class;
      Criterion : in Assignment_Criteria)
      return      Assignment.Object
   is
      pragma Unreferenced (This);
      A : Assignment.Object;
      P : Agpl.Htn.Plan.Object := Plan.Inflate; -- Fully expanded
   begin
--        Log ("T =" &
--             P.Enumerate_Tasks (Primitive => True, Pending   => True).Length'Img &
--             "; R =" & Agents.Length'Img, Always);

      --  Copy agents to the assignment.
      for I in Agents.First_Index .. Agents.Last_Index loop
         A.Set_Agent (Agents.Element (I));
      end loop;

      loop
         declare
            Pending : Htn.Tasks.Containers.Lists.List :=
                        P.Enumerate_Tasks (Primitive => True,
                                           Pending   => True);
            New_Ass  : Assignment.Object;
            Inserted : Htn.Tasks.Task_Id;
         begin
            --  Log ("Pending length is" & Pending.Length'Img, Always);
            exit when Pending.Is_Empty;

            Tasks.Insertions.Greedy (A,
                                     Pending,
                                     Costs,
                                     Criterion,
                                     New_Ass,
                                     Inserted);

            if Inserted /= Htn.Tasks.No_Task then
               Log ("Succesfully assigned task" & Inserted'Img, Debug,
                    Section => Log_Section);
               A := New_Ass;

               Htn.Plan.Utils.Trim_Or_Siblings (P, Inserted);
               Htn.Plan_Node.Set_Finished (P.Get_Node (Inserted));

--                 P.Print_Tree_Summary;
--                 A.Print_Assignment;
--                 Log ("Cost: " & To_String
--                      (Float (A.Get_Cost (Costs, Criterion))), Always);
            else
               raise Constraint_Error
                 with "No feasible assignment found for all tasks: " &
                      "remaining =" & Pending.Length'Img;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Agpl.Cr.Plan_Assigner.Greedy1;
