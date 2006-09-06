with Agpl.Cr.Agent.Containers;
with Agpl.Cr.Agent.Utils; use Agpl.Cr.Agent.Utils;
with Agpl.Htn.Tasks.Utils;
--  with Agpl.Text_Io; use Agpl.Text_Io;

package body Agpl.Cr.Tasks.Insertions is

   use type Cr.Costs;

   package Agent_Lists renames Agent.Containers.Lists;
   package Agent_Vectors renames Agent.Containers.Vectors;
   package Task_Lists renames Htn.Tasks.Containers.Lists;

   ---------------
   -- Before_Id --
   ---------------

   procedure Before_Id (List    : in out Htn.Tasks.Containers.Lists.List;
                        Job     : in     Htn.Tasks.Object'Class;
                        Id      : in     Htn.Tasks.Task_Id;
                        Is_Last : in     Boolean := False)
   is
      use Htn.Tasks.Containers.Lists;
      use type Htn.Tasks.Task_Id;
   begin
      if Is_Last then
         List.Append (Job);
      else
         declare
            I : Cursor := List.First;
         begin
            while Element (I).Get_Id /= Id loop
               Next (I);
            end loop;
            if Has_Element (I) then
               List.Insert (Before => I, New_Item => Job);
            else
               raise Program_Error;
            end if;
         end;
      end if;
   end Before_Id;

   ------------
   -- Greedy --
   ------------

   procedure Greedy
     (A          : in     Agent.Object'Class;
      T          : in     Htn.Tasks.Object'Class;
      Not_Before : in     Natural;
      New_Agent  :    out Agent.Handle.Object;
      Cost_Delta :    out Cr.Costs;
      Cost_Total :    out Cr.Costs;
      Success    :    out Boolean)
   is
      use Agpl.Htn.Tasks.Containers.Lists;
      New_Tasks : List := A.Get_Tasks;
   begin
      New_Agent.Set (A);

      if New_Tasks.Is_Empty or else Natural (New_Tasks.Length) <= Not_Before then
         Cost_Delta := A.Get_Cost (T);
         Cost_Total := Cost_Delta;
         Success    := True;
         declare
            Aux : Cr.Agent.Object'Class := A;
         begin
            Aux.Add_Task (T);
            New_Agent.Set (Aux);
         end;
      else
         declare
            Best_Pos    : Cursor := No_Element;
            --  Points to the point of insertion (next task of best point).
            Best_Cost   : Costs  := Costs'Last; -- New cost due to new task.
            Prev        : Cursor := No_Element;
            Curr        : Cursor := First (New_Tasks);
            Orig_Cost   : constant Costs :=
                            A.Get_Plan_Cost; -- Original plan cost.
         begin
            --  Skip Not_Before tasks:
            declare
               Counter : Natural := Not_Before;
            begin
               while Counter > 0 loop
                  Counter := Counter - 1;
                  Prev    := Curr;
                  Next (Curr);
               end loop;
            end;

            while Curr /= No_Element or else Prev /= No_Element loop
               declare
                  Curr_Cost   : Costs := Orig_Cost;
                  New_Cost_1  : Costs;
                  New_Cost_2  : Costs := 0.0;
               begin
                  --  First task special case:
                  if Prev = No_Element then
                     Curr_Cost  := Curr_Cost - A.Get_Cost (Element (Curr));
                     New_Cost_1 := A.Get_Cost (T);
                     New_Cost_2 := A.Get_Cost (T, Element (Curr));
                  elsif Curr = No_Element then -- Last task special case
                     New_Cost_1 := A.Get_Cost (Element (Prev), T);
                  else
                     Curr_Cost  := Curr_Cost -
                       A.Get_Cost (Element (Prev), Element (Curr));
                     New_Cost_1 := A.Get_Cost (Element (Prev), T);
                     New_Cost_2 := A.Get_Cost (T, Element (Curr));
                  end if;

                  if New_Cost_1 /= Costs'Last and then New_Cost_2 /= Costs'Last then
                     Curr_Cost := Curr_Cost + New_Cost_1 + New_Cost_2;

                     if Curr_Cost < Best_Cost then
                        Best_Cost := Curr_Cost;
                        Best_Pos  := Curr;
                     end if;
                  end if;
               end;

               Prev := Curr;
               Next (Curr);
            end loop;

            --  Insert at the best place:
            if Best_Cost = Costs'Last then
               Success := False;
            else
               Success    := True;
               Cost_Delta := Best_Cost - Orig_Cost;
               Cost_Total := Best_Cost;

               if Best_Pos = No_Element then -- last position
                  declare
                     Aux : Cr.Agent.Object'Class := A;
                  begin
                     Aux.Add_Task (T);
                     New_Agent.Set (Aux);
                  end;
               else
                  declare
                     Aux       : Cr.Agent.Object'Class := A;
                  begin
                     Htn.Tasks.Containers.Lists.Insert (New_Tasks, Best_Pos, T);
                     Aux.Set_Tasks (New_Tasks);
                     New_Agent.Set (Aux);
                  end;
               end if;
            end if;
         end;
      end if;
   end Greedy;

   ------------
   -- Greedy --
   ------------

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Htn.Tasks.Object'Class;
                     C          : in     Cost_Matrix.Object;
                     Not_Before : in     Natural;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Cr.Costs;
                     Cost_Total :    out Cr.Costs;
                     Success    :    out Boolean)
   is
      use Agpl.Htn.Tasks;
      use Agpl.Htn.Tasks.Containers.Lists;
      New_Tasks : List            := A.Get_Tasks;
      Name      : constant String := A.Get_Name;
   begin
      New_Agent.Set (A);

      if New_Tasks.Is_Empty or else Natural (New_Tasks.Length) < Not_Before then
         Cost_Delta := Cost_Matrix.Get_Cost
           (C, Name, Htn.Tasks.No_Task, Get_Id (T));
         Cost_Total := Cost_Delta;
         Success    := True;
         declare
            Aux : Cr.Agent.Object'Class := A;
         begin
            Aux.Add_Task (T);
            New_Agent.Set (Aux);
         end;
      else
         declare
            Best_Pos    : Cursor := No_Element;
            --  Points to the point of insertion (next task of best point).
            Best_Cost   : Costs  := Costs'Last; -- New cost due to new task.
            Prev        : Cursor := No_Element;
            Curr        : Cursor := First (New_Tasks);
            Orig_Cost   : constant Costs := Cost_Matrix.Get_Plan_Cost (C, A);
            --  Original plan cost.
         begin
            --  Skip Not_Before tasks:
            declare
               Counter : Natural := Not_Before;
            begin
               while Counter > 0 loop
                  Counter := Counter - 1;
                  Prev    := Curr;
                  Next (Curr);
               end loop;
            end;

            while Curr /= No_Element or else Prev /= No_Element loop
               declare
                  Curr_Cost   : Costs := Orig_Cost;
                  New_Cost_1  : Costs;
                  New_Cost_2  : Costs := 0.0;
               begin
                  --  First task special case:
                  if Prev = No_Element then
                     Curr_Cost  := Curr_Cost -
                       Cost_Matrix.Get_Cost (C, Name, Htn.Tasks.No_Task,
                                             Get_Id (Element (Curr)));
                     New_Cost_1 := Cost_Matrix.Get_Cost
                       (C, Name, Htn.Tasks.No_Task, Get_Id (T));
                     New_Cost_2 := Cost_Matrix.Get_Cost
                       (C, Name, Get_Id (T), Get_Id (Element (Curr)));
                  elsif Curr = No_Element then -- Last task special case
                     New_Cost_1 := Cost_Matrix.Get_Cost
                       (C, Name, Get_Id (Element (Prev)), Get_Id (T));
                  else
                     Curr_Cost  := Curr_Cost -
                       Cost_Matrix.Get_Cost
                         (C, Name,
                          Get_Id (Element (Prev)), Get_Id (Element (Curr)));
                     New_Cost_1 := Cost_Matrix.Get_Cost
                       (C, Name, Get_Id (Element (Prev)), Get_Id (T));
                     New_Cost_2 := Cost_matrix.Get_Cost
                       (C, Name, Get_Id (T), Get_Id (Element (Curr)));
                  end if;

                  if New_Cost_1 /= Cr.Infinite and then
                    New_Cost_2 /= Cr.Infinite
                  then
                     Curr_Cost := Curr_Cost + New_Cost_1 + New_Cost_2;

                     if Curr_Cost < Best_Cost then
                        Best_Cost := Curr_Cost;
                        Best_Pos  := Curr;
                     end if;
                  end if;
               end;

               Prev := Curr;
               Next (Curr);
            end loop;

            --  Insert at the best place:
            if Best_Cost = Cr.Infinite then
               Success := False;
            else
               Success    := True;
               Cost_Delta := Best_Cost - Orig_Cost;
               Cost_Total := Best_Cost;

               if Best_Pos = No_Element then -- last position
                  declare
                     Aux : Cr.Agent.Object'Class := A;
                  begin
                     Aux.Add_Task (T);
                     New_Agent.Set (Aux);
                  end;
               else
                  declare
                     Aux       : Cr.Agent.Object'Class := A;
                  begin
                     Htn.Tasks.Containers.Lists.Insert (New_Tasks, Best_Pos, T);
                     Aux.Set_Tasks (New_Tasks);
                     New_Agent.Set (Aux);
                  end;
               end if;
            end if;
         end;
      end if;
   end Greedy;

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Htn.Tasks.Object'Class;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Cr.Costs;
                     Cost_Total :    out Cr.Costs;
                     Success    :    out Boolean) is
   begin
      Greedy(A, T, 0, New_Agent, Cost_Delta, Cost_Total, Success);
   end Greedy;

   ------------
   -- Greedy --
   ------------

   procedure Greedy (Ass       : in     Assignment.Object;
                     T         : in     Htn.Tasks.Object'Class;
                     Costs     : in     Cost_Matrix.Object;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Success   :    out Boolean)
   is
      Agents : constant Agent.Containers.Lists.List := Ass.Get_Agents;

      Best_Agent : Agent.Handle.Object;
      Best_Cost  : Cr.Costs := Infinite;

      procedure Check_Agent (I : in Cr.Agent.Containers.Lists.Cursor) is
         New_Agent : Cr.Agent.Handle.Object;
         New_Total,
         New_Delta : Cr.Costs;
         New_Cost  : Cr.Costs;
         Success   : Boolean;
      begin
--           Put_Line ("Trying insertion of " & T.To_String & " at agent " &
--                     Agent.Containers.Lists.Element (I).Get_Name & " with tasks:");
--           Htn.Tasks.Utils.Print (Agent.Containers.Lists.Element (I).Get_Tasks);

         Greedy (Agent.Containers.Lists.Element (I),
                 T,
                 Costs,
                 0,
                 New_Agent,
                 Cost_Delta => New_Delta,
                 Cost_Total => New_Total,
                 Success    => Success);

         if Success then
            New_Cost := Evaluate (Criterion,
                                  Minmax => New_Total,
                                  Minsum => New_Delta);

            if New_Cost < Best_Cost then
               Best_Cost  := New_Cost;
               Best_Agent := New_Agent;
            end if;
         end if;
      end Check_Agent;
   begin
      New_Ass := Ass;

      Agent.Containers.Lists.Iterate (Agents, Check_Agent'Access);

      if Best_Agent.Is_Valid then
         Success := True;
         New_Ass.Set_Agent (Best_Agent.Get);
--           Put_Line ("Assigned to " & Best_Agent.Get.Get_Name);
      else
         Success := False;
      end if;
   end Greedy;

   ------------
   -- Greedy --
   ------------

   procedure Greedy (Ass       : in     Assignment.Object;
                     Tasks     : in     Htn.Tasks.Containers.Lists.List;
                     Costs     : in     Cost_Matrix.Object;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Inserted  :    out Htn.Tasks.Task_Id)
   is
      Pending : constant Htn.Tasks.Containers.Vectors.Vector :=
                  Htn.Tasks.Utils.To_Vector (Tasks);
      Best_Cost     : Cr.Costs := Infinite;
      pragma Optimization_Opportunity
        ("The new cost could be known without recomputing in full for each",
         "tried assignment, if the Greedy we are using passed more info out.");
   begin
      Inserted := Htn.Tasks.No_Task;

      for I in Pending.First_Index .. Pending.Last_Index loop
         declare
            Temp_Ass        : Assignment.Object;
            Partial_Success : Boolean;
            Temp_Cost       : Cr.Costs;
         begin
            Greedy (Ass,
                    Pending.Element (I),
                    Costs,
                    Criterion,
                    Temp_Ass,
                    Partial_Success);
            if Partial_Success then
               Temp_Cost := Temp_Ass.Get_Cost (Costs, Criterion);
               if Temp_Cost < Best_Cost then
                  New_Ass   := Temp_Ass;
                  Best_Cost := Temp_Cost;
                  Inserted  := Pending.Element (I).Get_Id;
               end if;
            end if;
         end;
      end loop;
   end Greedy;

   -----------------
   -- Greedy_Tail --
   -----------------

   procedure Greedy_Tail (Agent      : in Cr.Agent.Object'Class;
                          Tasks      : in Htn.Tasks.Containers.Lists.List;
                          Costs      : in     Cost_Matrix.Object;
                          New_Agent  :    out Cr.Agent.Handle.Object;
                          Inserted   :    out Htn.Tasks.Task_Id;
                          Cost_Total :    out Cr.Costs;
                          Cost_Delta :    out Cr.Costs)
   is
      use Task_Lists;

      Prev_Task : Htn.Tasks.Task_Id := Htn.Tasks.No_Task;
      Best_Cost : Cr.Costs := Infinite;

      procedure Check (I : in Cursor) is
         Cost : constant Cr.Costs := Cost_Matrix.Get_Cost (Costs,
                                                           Agent.Get_Name,
                                                           Prev_Task,
                                                           Element (I).Get_Id);
         Temp_Agent : Cr.Agent.Object'Class := Agent;
      begin
         if Cost < Best_Cost then
            Best_Cost := Cost;
            Temp_Agent.Set_Tasks (Agent.Get_Tasks);
            Temp_Agent.Add_Task (Element (I));
            New_Agent.Set (Temp_Agent);
            Inserted   := Element (I).Get_Id;
            Cost_Total := Cost_Matrix.Get_Plan_Cost (Costs, Temp_Agent);
            Cost_Delta := Cost;
         end if;
      end Check;
   begin
      Inserted := Htn.Tasks.No_Task;

      if Natural (Agent.Get_Tasks.Length) > 0 then
         Prev_Task := Agent.Get_Tasks.Last_Element.Get_Id;
      end if;

      Tasks.Iterate (Check'Access);
   end Greedy_Tail;

   -----------------
   -- Greedy_Tail --
   -----------------

   procedure Greedy_Tail (Ass        : in Assignment.Object;
                          T          : in Htn.Tasks.Object'Class;
                          Costs      : in Cost_Matrix.Object;
                          Criterion  : in     Assignment_Criteria;
                          New_Ass    :    out Assignment.Object;
                          Cost_Total :    out Cr.Costs;
                          Cost_Delta :    out Cr.Costs;
                          Success    :    out Boolean)
   is
      Agents  : Agent_Lists.List  := Ass.Get_Agents;

      Best_Cost  : Cr.Costs           := Infinite;
      Best_Agent : Agent_Lists.Cursor := Agent_Lists.No_Element;

      procedure Check_Agent (I : in Agent_Lists.Cursor) is
         Agent : Cr.Agent.Object'Class := Agent_Lists.Element (I);
         Tasks  : Task_Lists.List       := Agent.Get_Tasks;
         Tasks2 : Task_Lists.List       := Tasks;
         Agent_Total : Cr.Costs;
         Agent_Delta : Cr.Costs;
      begin
         Tasks2.Append (T);
         Agent_Total := Cost_Matrix.Get_Plan_Cost (Costs,
                                                   Agent.Get_Name,
                                                   Tasks2);
         Agent_Delta := Agent_Total - Cost_Matrix.Get_Plan_Cost (Costs,
                                                                 Agent.Get_Name,
                                                                 Tasks);
         if Evaluate (Criterion,
                      Minmax => Agent_Total,
                      Minsum => Agent_Delta) < Best_Cost then
            Best_Cost  := Evaluate (Criterion,
                                    Minmax => Agent_Total,
                                    Minsum => Agent_Delta);
            Best_Agent := I;
            Cost_Total := Agent_Total;
            Cost_Delta := Agent_Delta;
         end if;
      end Check_Agent;

   begin
      New_Ass := Ass;
      Agents.Iterate (Check_Agent'Access);
      if Best_Cost < Infinite then
         declare
            use Agent_Lists;
            Ag : Cr.Agent.Object'Class :=
                   Ass.Get_Agent (Element (Best_Agent).Get_Name);
         begin
            Ag.Add_Task (T);
            New_Ass.Set_Agent (Ag);
         end;
         Success := True;
      else
         Success := False;
      end if;
   end Greedy_Tail;

   -----------------
   -- Greedy_Tail --
   -----------------

   procedure Greedy_Tail (Ass       : in     Assignment.Object;
                          Tasks     : in     Htn.Tasks.Containers.Lists.List;
                          Costs     : in     Cost_Matrix.Object;
                          Criterion : in     Assignment_Criteria;
                          New_Ass   :    out Assignment.Object;
                          Inserted  :    out Htn.Tasks.Task_Id;
                          Cost_Total :    out Cr.Costs;
                          Cost_Delta :    out Cr.Costs)
   is
      Pending : constant Htn.Tasks.Containers.Vectors.Vector :=
                  Htn.Tasks.Utils.To_Vector (Tasks);
      Best_Cost     : Cr.Costs := Infinite;
   begin
      Inserted := Htn.Tasks.No_Task;

      for I in Pending.First_Index .. Pending.Last_Index loop
         declare
            Temp_Ass        : Assignment.Object;
            Partial_Success : Boolean;
            Temp_Cost,
            Temp_Total,
            Temp_Delta      : Cr.Costs;
         begin
            Greedy_Tail (Ass,
                         Pending.Element (I),
                         Costs,
                         Criterion,
                         Temp_Ass,
                         Temp_Total,
                         Temp_Delta,
                         Partial_Success);
            if Partial_Success then
               Temp_Cost := Evaluate (Criterion,
                                      Minmax => Temp_Total,
                                      Minsum => Temp_Delta);
               if Temp_Cost < Best_Cost then
                  New_Ass    := Temp_Ass;
                  Best_Cost  := Temp_Cost;
                  Inserted   := Pending.Element (I).Get_Id;
                  Cost_Total := Temp_Total;
                  Cost_Delta := Temp_Delta;
               end if;
            end if;
         end;
      end loop;
   end Greedy_Tail;

   ---------------
   -- Idle_Tail --
   ---------------

   procedure Idle_Tail (Ass        : in     Assignment.Object;
                        Tasks      : in     Htn.Tasks.Containers.Lists.List;
                        Costs      : in     Cost_Matrix.Object;
                        New_Ass    :    out Assignment.Object;
                        Inserted   :    out Htn.Tasks.Task_Id;
                        Cost_Total :    out Cr.Costs;
                        Cost_Delta :    out Cr.Costs)
   is
      Agents : constant Agent_Vectors.Vector := To_Vector (Ass.Get_Agents);

      Best_Cost  : Cr.Costs := Infinite;
      Best_Agent : Agent_Vectors.Cursor := Agent_Vectors.No_Element;

      procedure Check (I : Agent_Vectors.Cursor) is
         Agent_Cost : constant Cr.Costs :=
                        Cost_Matrix.Get_Plan_Cost (Costs,
                                                   Agent_Vectors.Element (I));
      begin
         if Agent_Cost < Best_Cost then
            Best_Cost  := Agent_Cost;
            Best_Agent := I;
         end if;
      end Check;

      Agent_Total,
      Agent_Delta : Cr.Costs;
      New_Agent   : Cr.Agent.Handle.Object;
      Agent_Task  : Htn.Tasks.Task_Id;
   begin
      Agents.Iterate (Check'Access);
      if Agent_Vectors.Has_Element (Best_Agent) then
         Greedy_Tail (Agent_Vectors.Element (Best_Agent),
                      Tasks,
                      Costs,
                      New_Agent,
                      Agent_Task,
                      Agent_Total,
                      Agent_Delta);
         New_Ass    := Ass;
         New_Ass.Set_Agent (New_Agent.Get);
         Inserted   := Agent_Task;
         Cost_Total := Agent_Total;
         Cost_Delta := Agent_Delta;
      else
         Inserted := Htn.Tasks.No_Task;
      end if;
   end Idle_Tail;

end Agpl.Cr.Tasks.Insertions;
