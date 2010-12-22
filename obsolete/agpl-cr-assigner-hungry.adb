 

with Agpl.Cr.Assignment;
with Agpl.Htn.Tasks;

package body Agpl.Cr.Assigner.Hungry is

   package Task_Lists renames Agpl.Htn.Tasks.Lists;
   use type Agent.Lists.Cursor;
   use type Task_Lists.Cursor;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Task_Lists.List)
      return Assignment.Object
   is
      pragma Unreferenced (This);

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

      function Less_Costly (Ag : in Agent.Object'Class)
                            return Task_Lists.Cursor
      is
         use Task_Lists;
         Best : Cursor := No_Element;
         Cost : Costs := Costs'Last;
         C    : Costs;
         I    : Cursor := First (Pending);
      begin
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

         return Best;
      end Less_Costly;

      ---------------
      -- Less_Used --
      ---------------
      --  Says agent with least acummulated cost.

      function Less_Used return Agent.Lists.Cursor is
         use Agent.Lists;
         I    : Cursor := First (Agts);
         Best : Cursor := No_Element;
         Used : Costs  := Costs'Last;
      begin
         while I /= No_Element loop
            if Agent.Get_Plan_Cost (Element (I)) <= Used then
               Used := Agent.Get_Plan_Cost (Element (I));
               Best := I;
            end if;
            Next (I);
         end loop;

         return Best;
      end Less_Used;

      Best      : Agent.Lists.Cursor;
      Best_Task : Task_Lists.Cursor;
   begin
      while not Pending.Is_Empty loop
         --  Select the less used agent.
         Best := Less_Used;
         if Best = Agent.Lists.No_Element then
            return Cr.Assignment.Invalid_Assignment;
         end if;

         --  Assing the less costly task to said agent.
         Best_Task := Less_Costly (Agent.Lists.Element (Best));
         if Best_Task = Task_Lists.No_Element then
            return Cr.Assignment.Invalid_Assignment;
         end if;

         declare
            procedure Assign (This : in out Agent.Object'Class) is
            begin
               Agent.Add_Task
                 (This, Task_Lists.Element (Best_Task));
            end Assign;
         begin
            Assignment.Add  (A, Agent.Lists.Element (Best),
                             Task_Lists.Element (Best_Task));
            Agent.Lists.Update_Element (Agts, Best, Assign'Access);
         end;

         --  Remove assigned task.
         Task_Lists.Delete (Pending, Best_Task);
      end loop;
      return A;
   end Assign;

end Agpl.Cr.Assigner.Hungry;
