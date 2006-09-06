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

with Agpl.Cr.Agent.Utils;
with Agpl.Cr.Tasks.Starting_Pose;
with Agpl.Htn.Tasks.Containers;
with Agpl.Optimization.Concorde;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Assigner.MTSP_Concorde is

   use Agpl.Optimization;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return      Assignment.Object
   is
      pragma Unreferenced (This);
      Jobs   : Htn.Tasks.Containers.Vectors.Vector;
      Ag     : constant Cr.Agent.Object'Class := Agents.First_Element;
      Result : Assignment.Object;
      Agvect : Agent.Containers.Vectors.Vector :=
                 Agent.Utils.To_Vector (Agents);

      --------------------
      -- Agent_For_Task --
      --------------------

      function Agent_For_Task (T : in Htn.Tasks.Object'Class)
                               return Agent.Object'Class
      is
      begin
         if T in Cr.Tasks.Starting_Pose.Object then
            for I in Agvect.First_Index .. Agvect.Last_Index loop
               if Agvect.Element (I).Get_Name =
                 Cr.Tasks.Starting_Pose.Object (T).Get_Name then
                  return Agvect.Element (I);
               end if;
            end loop;
            raise Program_Error;
         else
            return Ag;
         end if;
      end Agent_For_Task;

   begin
      --  Create the tasks vector
      --  Use a starting task for each agent:
      declare
         use Cr.Agent.Containers.Lists;
         I : Cursor := Agents.First;
      begin
         while Has_Element (I) loop
            Jobs.Append (Cr.Tasks.Starting_Pose.Create (Element (I).Get_Name));
            Next (I);
         end loop;
      end;

      --  Add the tasks:
      declare
         use Htn.Tasks.Containers.Lists;
         I : Cursor := Tasks.First;
      begin
         while Has_Element (I) loop
            if Element (I) in Cr.Tasks.Starting_Pose.Object then
               raise Constraint_Error with "No Starting_Pose tasks allowed";
            end if;
            Jobs.Append (Element (I));
            Next (I);
         end loop;
      end;

      --  Create the concorde things and solve
      declare
         use Optimization.Concorde;
         Start : Start_Matrix (1 .. Salesmen (Agents.Length));
         C     : Optimization.Concorde.Cost_Matrix :=
                   Cost_Matrices.Create (Cities (Jobs.Length),
                                         Cities (Jobs.Length));
         use Cost_Matrix;
      begin
         for I in Start'Range loop
            Start (I) := Cities (I);
         end loop;

         for I in Jobs.First_Index .. Jobs.Last_Index loop
            declare
               --  Choose the agent for the starting task
               --  If is not a Starting_Task, any will do:
               Apt_Agent : constant Agent.Object'Class :=
                             Agent_For_Task (Jobs.Element (I));
            begin
               for J in Jobs.First_Index .. Jobs.Last_Index loop
                  if Get_Cost (Costs,
                               Apt_Agent.Get_Name,
                               Jobs.Element (I).Get_Id,
                               Jobs.Element (J).Get_Id) < Infinite
                  then
                     C.Set
                       (Cities (I),
                        Cities (J),
                        Concorde.Costs
                          (Get_Cost (Costs,
                           Apt_Agent.Get_Name,
                           Jobs.Element (I).Get_Id,
                           Jobs.Element (J).Get_Id)));
                  else
                     C.Set (Cities (I),
                            Cities (J),
                            Concorde.Inf);
                  end if;
               end loop;
            end;
         end loop;

--         Optimization.Concorde.Print_Problem (C);

         declare
            --  Solve
            Tour : constant Normal_Tour :=
                     Create (Start,
                             Solve_Mtsp (Start,
                                         C,
                                         No_Return => True));
            use Cr.Agent.Containers.Lists;
            A : Cursor := Agents.First;
         begin
            --  Reconstruct agents
            for I in 1 .. Tour.Last loop
               declare
                  New_Agent : Cr.Agent.Object'Class := Element (A);
               begin
                  Next (A);
                  --  Set the name of the starting task, I think the tour
                  --  can be rotated (error somewhere????)
                  declare
                     Name : constant String :=
                              Cr.Tasks.Starting_Pose.Object
                                (Jobs.Element
                                   (Positive (Tour.City (I, 1)))).Get_Name;
                  begin
                     if Name /= New_Agent.Get_Name then
                        Log ("Agent mismatch! " & Name & " should be " &
                             New_Agent.Get_Name, Error);
                        raise Constraint_Error;
                     end if;
                  end;

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
