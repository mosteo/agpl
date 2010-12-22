 

with Agpl.Cr.Assignment;
with Agpl.Htn.Tasks;
with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Assigner.Hungry2 is

   package Task_Lists renames Agpl.Htn.Tasks.Lists;
   use type Agent.Lists.Cursor;
   use type Task_Lists.Cursor;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Task_Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return Assignment.Object
   is
      pragma Unreferenced (This, Costs);

      A : Assignment.Object;
      --  The result we'll return.

      Agts  : Agent.Lists.List := Agents;
      --  Modifiable copy.

      Pending : Task_Lists.List := Tasks;
      --  Tasks not yet assigned.

      -----------------
      -- Less_Costly --
      -----------------
      --  Says best least costly task for a given agent.
      --  Best can be No_Element if no proper task for the agent.

      procedure Less_Costly
        (Ag   : in     Agent.Object'Class;
         Best :    out Htn.Tasks.Lists.Cursor;
         Cost :    out Cr.Costs)
      is
         use Task_Lists;
         C    : Cr.Costs;
         I    : Cursor := First (Pending);
      begin
         Cost := Cr.Costs'Last;
         while I /= No_Element loop
            if Agent.Has_Tasks (Ag) then
               C := Agent.Get_Cost (Ag, Agent.Get_Last_Task (Ag), Element (I));
            else
               C := Agent.Get_Cost (Ag, Element (I));
            end if;
            if C < Cost then
               Cost := C;
               Best := I;
            end if;

            Next (I);
         end loop;
      end Less_Costly;

      ----------------
      -- Best_Combo --
      ----------------

      procedure Best_Combo (Best_Agent : out Agent.Lists.Cursor;
                            Best_Task  : out Htn.Tasks.Lists.Cursor)
      is
         use Agent.Lists;
         I         : Cursor   := First (Agts);
         Best_Cost : Cr.Costs := Cr.Costs'Last;
      begin
         while I /= No_Element loop
            declare
               Agent_Cost : Cr.Costs;
               Agent_Task : Htn.Tasks.Lists.Cursor;
            begin
               Less_Costly (Element (I), Agent_Task, Agent_Cost);
               if Agent_Task /= Task_Lists.No_Element and then
                  Agent_Cost < Best_Cost
               then
                  Best_Agent := I;
                  Best_Task  := Agent_Task;
                  Best_Cost  := Agent_Cost;
               end if;
            end;
            Next (I);
         end loop;

--           if Has_Element (Best_Agent) then
--              Log ("Best Agent: " & Element (Best_Agent).Get_Name, Always);
--              Log ("Best Task : " & Htn.Tasks.Lists.Element (Best_Task).To_String, Always);
--              Log ("Best Cost :" & Strings.To_String (Float (Best_Cost)), Always);
--           end if;
      end Best_Combo;

      Best_Agent : Agent.Lists.Cursor;
      Best_Task  : Task_Lists.Cursor;
   begin
      while not Pending.Is_Empty loop

         --  Select the agent with a less costly task:
         Best_Combo (Best_Agent, Best_Task);

         if Best_Agent = Agent.Lists.No_Element then
            return Cr.Assignment.Invalid_Assignment;
         end if;

         declare
            procedure Assign (This : in out Agent.Object'Class) is
            begin
               Agent.Add_Task
                 (This, Task_Lists.Element (Best_Task));
            end Assign;
         begin
            Assignment.Add  (A,
                             Agent.Lists.Element (Best_Agent),
                             Task_Lists.Element (Best_Task));
            Agent.Lists.Update_Element (Best_Agent, Assign'Access);
         end;

         --  Remove assigned task.
         Task_Lists.Delete (Pending, Best_Task);
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Agpl.Cr.Assigner.Hungry2;
