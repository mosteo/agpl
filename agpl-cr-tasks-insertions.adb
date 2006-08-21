with Agpl.Cr.Agent.Lists;
with Agpl.Htn.Tasks.Lists_Utils;
with Agpl.Htn.Tasks.Vectors;

package body Agpl.Cr.Tasks.Insertions is

   use type Cr.Costs;

   ---------------
   -- Before_Id --
   ---------------

   procedure Before_Id (List    : in out Htn.Tasks.Lists.List;
                        Job     : in     Htn.Tasks.Object'Class;
                        Id      : in     Htn.Tasks.Task_Id;
                        Is_Last : in     Boolean := False)
   is
      use Htn.Tasks.Lists;
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
      use Agpl.Htn.Tasks.Lists;
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
                     Htn.Tasks.Lists.Insert (New_Tasks, Best_Pos, T);
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
      use Agpl.Htn.Tasks.Lists;
      New_Tasks : List            := A.Get_Tasks;
      Name      : constant String := A.Get_Name;
   begin
      New_Agent.Set (A);

      if New_Tasks.Is_Empty or else Natural (New_Tasks.Length) <= Not_Before then
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
                     Htn.Tasks.Lists.Insert (New_Tasks, Best_Pos, T);
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
      Agents : constant Agent.Lists.List := Ass.Get_Agents;

      Best_Agent : Agent.Handle.Object;
      Best_Cost  : Cr.Costs := Infinite;

      procedure Check_Agent (I : in Cr.Agent.Lists.Cursor) is
         New_Agent : Cr.Agent.Handle.Object;
         New_Total,
         New_Delta : Cr.Costs;
         New_Cost  : Cr.Costs;
         Success   : Boolean;
      begin
--           Put_Line ("Trying insertion of " & T.To_String & " at agent " &
--                     Agent.Lists.Element (I).Get_Name & " with tasks:");
--           Htn.Tasks.Lists_Utils.Print (Agent.Lists.Element (I).Get_Tasks);

         Greedy (Agent.Lists.Element (I),
                 T,
                 Costs,
                 0,
                 New_Agent,
                 Cost_Delta => New_Delta,
                 Cost_Total => New_Total,
                 Success    => Success);

         if Success then
            New_Cost := Evaluate (Criterion,
                                  Minimax  => New_Total,
                                  Totalsum => New_Delta);

            if New_Cost < Best_Cost then
               Best_Cost  := New_Cost;
               Best_Agent := New_Agent;
            end if;
         end if;
      end Check_Agent;
   begin
      New_Ass := Ass;

      Agent.Lists.Iterate (Agents, Check_Agent'Access);

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
                     Tasks     : in     Htn.Tasks.Lists.List;
                     Costs     : in     Cost_Matrix.Object;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Inserted  :    out Htn.Tasks.Task_Id)
   is
      Pending : constant Htn.Tasks.Vectors.Vector :=
                  Htn.Tasks.Lists_Utils.To_Vector (Tasks);
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

end Agpl.Cr.Tasks.Insertions;
