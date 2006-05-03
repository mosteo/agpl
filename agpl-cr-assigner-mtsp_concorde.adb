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

with Agpl.Cr.Tasks.Starting_Pose;
with Agpl.Htn.Tasks.Vectors;
with Agpl.Optimization.Concorde;

package body Agpl.Cr.Assigner.MTSP_Concorde is

   use Agpl.Optimization;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return      Assignment.Object
   is
      pragma Unreferenced (This, Costs);
      Jobs   : Htn.Tasks.Vectors.Vector;
      Ag     : constant Cr.Agent.Object'Class := Agents.First_Element;
      Result : Assignment.Object;
   begin
      --  Create the tasks vector
      --  Use a starting pose for each agent:
      declare
         use Cr.Agent.Lists;
         I : Cursor := Agents.First;
      begin
         while Has_Element (I) loop
            Jobs.Append (Cr.Tasks.Starting_Pose.Create (Element (I).Get_Name));
            Next (I);
         end loop;
      end;

      --  Add the tasks:
      declare
         use Htn.Tasks.Lists;
         I : Cursor := Tasks.First;
      begin
         while Has_Element (I) loop
            Jobs.Append (Element (I));
            Next (I);
         end loop;
      end;

      --  Create the concorde things and solve
      declare
         use Optimization.Concorde;
         Start : Start_Matrix (1 .. Salesmen (Agents.Length));
         C     : Optimization.Concorde.Cost_Matrix (1 .. Cities (Jobs.Length),
                                                    1 .. Cities (Jobs.Length));
      begin
         for I in Start'Range loop
            Start (I) := Cities (I);
         end loop;

         for I in Jobs.First_Index .. Jobs.Last_Index loop
            for J in Jobs.First_Index .. Jobs.Last_Index loop
               if Ag.Get_Cost (Jobs.Element (I),
                               Jobs.Element (J)) < Cr.Costs'Last
               then
                  C (Cities (I),
                     Cities (J)) :=
                    Concorde.Costs (Ag.Get_Cost (Jobs.Element (I),
                                                 Jobs.Element (J)));
               else
                  C (Cities (I),
                     Cities (J)) := Concorde.Inf;
               end if;
            end loop;
         end loop;

         declare
            --  Solve
            Tour : constant Normal_Tour :=
                     Create (Start,
                             Solve_Mtsp (Start,
                                         C,
                                         No_Return => True));
            use Cr.Agent.Lists;
            A : Cursor := Agents.First;
         begin
            --  Reconstruct agents
            for I in 1 .. Tour.Last loop
               declare
                  New_Agent : Cr.Agent.Object'Class := Element (A);
               begin
                  Next (A);
                  --  Assign tasks, skipping the forced starting pose
                  for J in 2 .. Tour.Last (I) loop
                     New_Agent.Add_Task (Jobs.Element
                                           (Positive (Tour.City (I, J))));
                  end loop;
                  Result.set_Agent (New_Agent);
               end;
            end loop;
         end;
      end;

      return Result;
   end Assign;

end Agpl.Cr.Assigner.MTSP_Concorde;
