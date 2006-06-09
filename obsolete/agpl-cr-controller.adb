with Agpl.Calendar.Format;
with Agpl.Constants;
with Agpl.Cr.Assignment;
with Agpl.Htn.Tasks.Lists;
with Agpl.Htn.Tasks.Primitive;
with Agpl.Strings;
with Agpl.Trace;

with Ada.Text_Io;

package body Agpl.Cr.Controller is

   ------------
   -- Object --
   ------------

   protected body Object is

      procedure Notify_Observers;

      ---------
      -- Add --
      ---------

      procedure Add (This : in Assigner.Object'Class) is
      begin
         Demiurge.Add (Demi, This);
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (This : in Agent.Object'Class) is
      begin
         Agent.Lists.Append (Agents, This);
         Agpl.Trace.Log
           ("Agent " & Agent.Get_Name (This) & " added to controller.",
            Agpl.Trace.Debug,
            Agpl.Constants.CR);
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (This : in Htn.Plan.Object) is
      begin
         Plan := This;
         Replan;
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (This : in Htn.Tasks.Object'Class) is
      begin
         Htn.Plan.Add_Task (Plan, This);
         Replan;
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (This : in Controller_Observer.Object'Class) is
      begin
         Controller_Observer.Lists.Append (Observers, This);
      end Add;

      ----------------------
      -- Check_Completion --
      ----------------------

      function Check_Completion return Boolean is
         use Agent.Lists;
         Done : Boolean := True;
      begin
         if not Assign_Ok then
            return False;
         end if;

         declare
            I : Cursor := First (Agents);
         begin
            while I /= No_Element loop
               Done := Done and not Agent.Has_Tasks (Element (I));
               exit when not Done;
               Next (I);
            end loop;
         end;

         return Done;
      end Check_Completion;

      --------------
      -- Get_Plan --
      --------------

      function Get_Plan return Htn.Plan.Object is
      begin
         return Plan;
      end Get_Plan;

      ---------------
      -- Mark_Done --
      ---------------

      procedure Mark_Done
        (A : in Agent.Object'Class; T : in Htn.Tasks.Object'Class) is
         procedure Remove (A : in out Agent.Object'Class) is
         begin
            Agent.Remove_Task (A, Htn.Tasks.Get_Id (T));
         end Remove;
         use Ada.Calendar;
         use Agent.Lists;
         I : Cursor := First (Agents);
      begin
         while I /= No_Element loop
            if Agent.Get_Name (Element (I)) = Agent.Get_Name (A) then
               Agpl.Trace.Log ("Agent " & Agent.Get_Name (Element (I)) &
                          " has finished task " &
                          Agpl.Htn.Tasks.To_String (T), Trace.Debug);
               Update_Element (I, Remove'Access);
               if not Agent.Has_Tasks (Element (I)) then
                  Trace.Log ("Agent " & Agent.Get_Name (Element (I)) &
                             " has finished all its tasks", Trace.Debug);
                  Trace.Log ("Estimated: " &
                             Agpl.Calendar.Format.Image
                               (Duration (Agent.Get_Cached_Cost (Element (I)))),
                             Trace.Debug);
                  Trace.Log ("Real:      " &
                             Agpl.Calendar.Format.Image
                               (Agent.Get_Elapsed (Element (I))),
                             Trace.Debug);
               end if;
               exit;
            end if;
            Next (I);
         end loop;

         --  Get new plans if necessary:
         declare
            Plans : Htn.Plan.Lists.List;

            --------------
            -- New_Plan --
            --------------

            procedure New_Plans (This : in Htn.Plan.Object) is
            begin
               Plans.Append (This);
            end New_Plans;

         begin
            Htn.Plan.Mark_Task_Done (Plan,
                                Htn.Tasks.Get_Id (T),
                                New_Plans'Unrestricted_Access);
            if not Plans.Is_Empty then
               Replace_Plan (Plans);
            else
               --  No change in plan, but a task is newly finished.
               Notify_Observers;
            end if;
         end;

         if Check_Completion then
            Trace.Log ("PLAN EXECUTED.", Trace.Informative);
            Trace.Log ("Estimated: " &
                       Agpl.Calendar.Format.Image (Duration (Plan_Cost)),
                       Trace.Informative);
            Trace.Log ("Real:      " &
                       Agpl.Calendar.Format.Image (Clock - Plan_Start),
                       Trace.Informative);
         end if;
      end Mark_Done;

      procedure Notify_Observers is
      begin
         declare
            procedure Notify (X : in out Controller_Observer.Object'Class) is
            begin
               X.Plan_Changed (Plan);
            end Notify;
            use Controller_Observer.Lists;
            I : Controller_Observer.Lists.Cursor := First (Observers);
         begin
            while I /= Controller_Observer.Lists.No_Element loop
               Update_Element (I, Notify'Access);
               Next (I);
            end loop;
         end;
      end Notify_Observers;

      --------------------------
      -- Pass_Tasks_To_Agents --
      --------------------------

      procedure Pass_Tasks_To_Agents is
      begin
         --  Copy tasks to agents.
         if Assign_Ok then
            declare
               use Agent.Lists;
               I : Cursor := First (Agents);
               T : Htn.Tasks.Lists.List;
            begin
               while I /= No_Element loop
                  T := Assignment.Get_Tasks (Assignments, Element (I));
                  declare
                     procedure Set_Tasks (This : in out Agent.Object'Class) is
                     begin
                        Agent.Set_Tasks (This, T);
                        Agent.Mark_Start (This);
                     end Set_Tasks;
                  begin
                     Update_Element (I, Set_Tasks'Access);
                  end;
                  Next (I);
               end loop;
            end;
         end if;
      end Pass_Tasks_To_Agents;

      ------------------
      -- Replace_Plan --
      ------------------

      procedure Replace_Plan (This : in Htn.Plan.Object) is
      begin
         Plan := This;
         Replan;
      end Replace_Plan;

      ------------------
      -- Replace_Plan --
      ------------------

      procedure Replace_Plan (Plans : in Htn.Plan.Lists.List) is
         Best          : Costs := Costs'Last;
         Best_For_Plan : Costs;

         ----------------
         -- Plan_Found --
         ----------------

         procedure Plan_Found (P : in Htn.Plan.Object) is

            ----------------------
            -- Assignment_Found --
            ----------------------

            procedure Assignment_Found (A : in Assignment.Object) is
               Cost : constant Costs := Assignment.Get_Max_Min_Cost (A);
            begin
               if Cr.Assignment.Is_Valid (A) then
                  if Cost < Best then
                     Best          := Cost;
                     Plan          := P; -- Redundancy when assignments are for the same plan.
                     Assignments   := A;
                     Assign_Ok     := True;
                     Plan_Start    := Ada.Calendar.Clock;
                  end if;
                  Best_For_Plan := Costs'Min (Best_For_Plan, Cost);
                  Trace.Log ("Cost found: " & Strings.To_String (Float (Cost)),
                             Trace.Debug, Constants.Cr);
               else
                  Trace.Log ("Assigner failed", Trace.Debug, Constants.Cr);
               end if;
            end Assignment_Found;

            use Agpl;
         begin
            Best_For_Plan := Costs'Last;
            --  Htn.Plan.Print_Summary (P);
            Demiurge.Assign
              (Demi, Agents, Htn.Plan.Get_Tasks (P),
               Assignment_Found'Unrestricted_Access);

            if Best_For_Plan < Costs'Last then
               Trace.Log
                 ("Plan found with best cost: " &
                  Duration'Image (Duration (Best_For_Plan)),
                  Trace.Informative,
                  Constants.CR);
            else
               Trace.Log
                 ("Unable to assign plan to agents: no assigner succeeded.",
                  Trace.Informative,
                  Constants.CR);
            end if;
         end Plan_Found;

         use Htn.Plan.Lists;
         I : Cursor := First (Plans);
      begin
         Assign_Ok := False;

         while I /= No_Element loop
            Htn.Plan.Expand (Element (I), Plan_Found'Unrestricted_Access);
            Next (I);
         end loop;

         --  Keep estimated cost
         if Assign_Ok then
            Plan_Cost := Assignment.Get_Max_Min_Cost (Assignments);
            Pass_Tasks_To_Agents;

            --  Set Owners
            Assignment.Fill_Owners (Assignments, Plan);

            --  Notify observers
            Notify_Observers;
         end if;
      end Replace_Plan;

      ------------
      -- Replan --
      ------------

      procedure Replan is
         Single_Plan : Htn.Plan.Lists.List;
      begin
         Single_Plan.Append (Plan);
         Replace_Plan (Single_Plan);
      end Replan;

      ---------
      -- Run --
      ---------

      procedure Run is

         Done    : Boolean;

         -------------
         -- Process --
         -------------

         procedure Process (A : in out Agent.Object'Class) is
         begin
            if Agent.Has_Tasks (A) then
               Agent.Execute
                 (A,
                  Htn.Tasks.Primitive.Object'Class
                    (Htn.Plan.Get_Task
                       (Plan,
                       Htn.Tasks.Get_Id (Agent.Get_First_Task (A))).all),
                  Plan,
                  Done);
            else
               Agent.Execute_When_Idle (A, Plan);
            end if;
         end Process;

      begin
         declare
            use Agent.Lists;
            I    : cursor := First (Agents);
         begin
            while I /= No_Element loop
               Done := False;
               Update_Element (I, Process'Access);

               --  Mark the task as finished for the robot
               if Done then
                  Mark_Done (Element (I), Agent.Get_First_Task (Element (I)));
               end if;

               --  Abort current round if plan has changed!
               if Htn.Plan.Is_Modified (Plan) then
                  Htn.Plan.Set_Unmodified (Plan);
                  Replan;
                  return;
               end if;

               Next (I);
            end loop;
         end;
      end Run;

      ------------------
      -- Print_Report --
      ------------------

      procedure Print_Report is
         use Ada.Text_Io;

         procedure Print_Agent_Tasks (This : in Agent.Object'Class)
         is
            use Htn.Tasks.Lists;
            Tasks : constant List := Agent.Get_Tasks (This);
            I : Cursor := First (Tasks);
         begin
            while I /= No_Element loop
               Put (Htn.Tasks.To_String (Element (I)) & ", ");
               Next (I);
            end loop;
            New_Line;
            Put_Line ("Total Cost: " & Costs'Image (Agent.Get_Plan_Cost (This)));
         end Print_Agent_Tasks;

      begin
         Put_Line ("*****************************");
         Put_Line ("Assignation: " & Assign_Ok'Img);

         declare
            use Agent.Lists;
            I : Cursor := First (Agents);
         begin
            while I /= No_Element loop
               Put_Line ("Agent " & Agent.Get_Name (Element (I)) & " TO DO:");
               Print_Agent_Tasks (Element (I));
               Next (I);
            end loop;
         end;
      end Print_Report;

   end Object;

end Agpl.Cr.Controller;
