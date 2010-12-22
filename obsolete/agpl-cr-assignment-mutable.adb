with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Assignment.Mutable is

   Flip_Probability : constant Float := 0.5;

   use Agpl.Optimization.Annealing;

   procedure Insert_At_Random (This : in out Object;
                               Job  : in     Htn.Tasks.Object'Class)
   is
      use Cr.Agent.Maps;

      Agent_Count  : constant Natural := Natural (This.Agents.Length);
      Target_Agent :          Natural := Natural (Random.Get_Integer
                                                    (0, Agent_Count));
      --  Goes from 0 .. Agent_Count - 1
      I            : Cursor := This.Agents.First;
   begin
         --  Reach the interesting agent:
      while Target_Agent > 0 loop
         Target_Agent := Target_Agent - 1;
         Next (I);
      end loop;

      --  It could happen, for a 1.0 random result in the generator, to go over
      --  the board:
      if not Has_Element (I) then
         I := This.Agents.First;
      end if;

      declare
         procedure Insert_Task (K : in String; A : in out Agent.Object'Class) is

            procedure Do_It (Tasks : in out Htn.Tasks.Lists.List) is
               use Htn.Tasks.Lists;
               Task_Count  : constant Natural := Natural (Tasks.Length);
               Target_Task :          Natural :=
                               Random.Get_Integer (0, Task_Count);
               I           :          Htn.Tasks.Lists.Cursor := Tasks.First;
            begin
               Log ("Inserting at pos" & Natural'Image (Target_Task + 1) &
                    " of" & Natural'Image (Task_Count + 1) &
                    " in agent " & K, Always);
               while Target_Task > 0 loop
                  Target_Task := Target_Task - 1;
                  if not Has_Element (I) then
                     I := Tasks.First;
                  end if;
                  Next (I);
                  Tasks.Insert (New_Item => Job, Before => I);
               end loop;
            end Do_It;

         begin
            A.Modify_Task_List (Do_It'Access);
         end Insert_Task;
      begin
         Update_Element (This.Agents, I, Insert_Task'Access);
      end;
   end Insert_At_Random;

   ------------------------
   -- Invalid_Assignment --
   ------------------------

   function Invalid_Assignment return Object is
      This : Object;
   begin
      This.Ok := False;
      return This;
   end Invalid_Assignment;

   ------------
   -- Mutate --
   ------------

   function Mutate (This : in Object) return Object is
      Result : Object := This;
      Job    : Htn.Tasks.Handle.Object;
   begin
      Select_Random_Task (Result, Job);

      if Job.Is_Valid then
         if Random.Uniform < Flip_Probability then
            Job.Set (Flip (Htn.Tasks.Primitive.Object (Job.Get)));
            Log ("Task flipped", Always);
         end if;

         Insert_At_Random (Result, Htn.Tasks.Primitive.Object (Job.Get));

         return Result;
      else
         Log ("No mutation found", Always);
         return This;
      end if;
   end Mutate;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (Old_Cost,
      New_Cost : in Cost;
      Temp     : in Temperature)
      return        Acceptability
   is
   begin
      if New_Cost < Old_Cost then
         return Acceptability'Last;
      else
         return Acceptability (Old_Cost / New_Cost) *
           Acceptability (Temp);
      end if;
   end Normalize;

   ------------------------
   -- Select_Random_Task --
   ------------------------

   procedure Select_Random_Task (This : in out Object;
                                 Job  :    out Htn.Tasks.Handle.Object)
   is
      Agent_Count  : constant Natural := Natural (This.Agents.Length);
      Target_Agent :          Natural :=
                       Random.Get_Integer (0, Agent_Count - 1);
      use Cr.Agent.Maps;
      I            : Cursor := This.Agents.First;
   begin
      Job.Clear;

      --  Reach the interesting agent:
      while Target_Agent > 0 loop
         Target_Agent := Target_Agent - 1;
         Next (I);
      end loop;

      --  It could happen, for a 1.0 random result in the generator, to go over
      --  the board:
      if not Has_Element (I) then
         I := This.Agents.First;
      end if;

      declare
         procedure Get_Task (K : in String; A : in out Agent.Object'Class) is
            --  Selects a random task from the agent and removes from his list.
            --  It could happen the agent is idle, and then

            procedure Select_Task (Tasks : in out Htn.Tasks.Lists.List) is
               use Htn.Tasks.Lists;
               use Htn.Tasks.Handle;
               Task_Count  : constant Natural := Natural (Tasks.Length);
               Target_Task :          Natural :=
                               Random.Get_Integer (0, Task_Count - 1);
               I           :          Htn.Tasks.Lists.Cursor := Tasks.First;
            begin
               if Task_Count > 0 then
                  while Target_Task > 0 loop
                     Target_Task := Target_Task - 1;
                     Next (I);
                     if not Has_Element (I) then
                        I := Tasks.First;
                     end if;
                  end loop;
                  Log ("Chosen task " & Htn.Tasks.To_String (Element (I)) &
                       " from agent " & K, Always);
                  Set (Job, Element (I));
                  Tasks.Delete (I);
               end if;
            end Select_Task;

         begin
            A.Modify_Task_List (Select_Task'Access);
         end Get_Task;
      begin
         Update_Element (I, Get_Task'Access);
      end;
   end Select_Random_Task;

end Agpl.Cr.Assignment.Mutable;
