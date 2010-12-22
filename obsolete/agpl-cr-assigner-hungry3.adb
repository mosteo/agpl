 

with Agpl.Cr.Agent.Handle;
with Agpl.Cr.Assignment;
with Agpl.Cr.Tasks.Insertions;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Handle;
--  with Agpl.Trace; use Agpl.Trace;

with Ada.Containers.Indefinite_Ordered_Maps;

package body Agpl.Cr.Assigner.Hungry3 is

   package Task_Lists renames Agpl.Htn.Tasks.Lists;
   use type Agent.Lists.Cursor;
   use type Task_Lists.Cursor;
   use type Htn.Tasks.Task_Id;
   use type Cr.Agent.Handle.Object;
   use type Htn.Tasks.Handle.Object;

   package Int_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Natural);

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
      A : Assignment.Object;
      --  The result we'll return.

      Pending : Task_Lists.List := Tasks;
      --  Tasks not yet assigned.

      Not_Before : Int_Maps.Map;
      --  To keep track of untouchable tasks if This.Keep_Order

      -------------------------
      -- Remove_From_Pending --
      -------------------------

      procedure Remove_From_Pending (T : in Htn.Tasks.Object'Class) is
         use Task_Lists;
         I : Cursor := Pending.First;
      begin
         while Has_Element (I) loop
            if Element (I).Get_Id = T.Get_Id then
               Pending.Delete (I);
               return;
            else
               Next (I);
            end if;
         end loop;
         raise Program_Error; -- Shouldn't be reached.
      end Remove_From_Pending;

      ---------------
      -- Try_Agent --
      ---------------
      --  Try all pending tasks in the agent.
      --  Returns a new agent with the task inserted at best place.
      --  Ct holds the new total cost for the modified agent.
      procedure Try_Agent (Ag  : in     Agent.Object'Class;
                           Nw  :    out Agent.Handle.Object;
                           Ct  :    out Cr.Costs;
                           Job :    out Htn.Tasks.Handle.Object)
      is
         use Task_Lists;
         T         : Task_Lists.Cursor  := Pending.First;
      begin
         Ct := Cr.Costs'Last;
         while Has_Element (T) loop
            declare
               Try_Agent : Agent.Handle.Object;
               Dummy     : Cr.Costs;
               Try_Total : Cr.Costs;
               Ok        : Boolean;
            begin
               Cr.Tasks.Insertions.Greedy
                 (Ag,
                  Element (T),
                  Costs,
                  Int_Maps.Element (Not_Before.Find (Ag.Get_Name)),
                  Try_Agent,
                  Cost_Delta => Dummy,
                  Cost_Total => Try_Total,
                  Success    => Ok);

               if Ok and then Try_Total < Ct then
                  Ct  := Try_Total;
                  Nw  := Try_Agent;
                  Job.Set (Element (T));
               end if;
            end;
            Next (T);
         end loop;
      end Try_Agent;

   begin
      --  Initialize assignment:
      declare
         use Agent.Lists;
         I : Cursor := Agents.First;
      begin
         while Has_Element (I) loop
            if This.Keep_Order then
               Not_Before.Include (Element (I).Get_Name,
                                   Natural (Element (I).Get_Tasks.Length));
            else
               Not_Before.Include (Element (I).Get_Name,
                                   0);
            end if;

            A.Set_Agent (Element (I));
            Next (I);
         end loop;
      end;

      --  Assign tasks:
      while not Pending.Is_Empty loop
         declare
            Best_Cost        : Cr.Costs := Cr.Costs'Last;
            Best_Agent       : Agent.Handle.Object;
            Best_Task        : Htn.Tasks.Handle.Object;
            use Agent.Lists;
            I                : Cursor := Agents.First;
         begin
            while Has_Element (I) loop
               declare
                  Mod_Agent : Agent.Handle.Object;
                  Mod_Cost  : Cr.Costs;
                  Target    : Htn.Tasks.Handle.Object;
               begin
                  --  Select the best task for a given agent
                  declare
                     Name : constant String := Element (I).Get_Name;
                  begin
                     Try_Agent (A.Get_Agent (Name),
                                Mod_Agent, Mod_Cost, Target);
                  end;

                  if Target.Is_Valid then
                     if Mod_Cost < Best_Cost then
                        Best_Cost  := Mod_Cost;
                        Best_Agent.Set (Mod_Agent.Get);
                        Best_Task.Set (Target.Get);
                     end if;
                  end if;
               end;
               Next (I);
            end loop;

            if Best_Agent.Is_Valid then
               A.Set_Agent (Best_Agent.Get);
               Remove_From_Pending (Best_Task.Get);
            else
               A.Set_Valid (False);
               return A;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Agpl.Cr.Assigner.Hungry3;
