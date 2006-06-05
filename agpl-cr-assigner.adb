with Agpl.Htn.Plan_Node;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Agpl.Cr.Assigner is

   use type Cr.Costs;

   use type Htn.Plan.Subplan;
   use type Htn.Plan_Node.Node_Kind;

   procedure Assign_Best_Plan
     (The_Assigner   : in     Object'Class;
      Agents         : in     Agent.Lists.List;
      Plans          : in     Htn.Plan.Object; -- OR of possible plans.
      Criterion      : in     Assignment_Criteria;
      Plan           :    out Htn.Plan.Object; -- Selected plan with best cost.
      The_Assignment :    out Assignment.Object)
   is
   begin
      Assign_Best_Plan (The_Assigner,
                        Agents,
                        Plans,
                        Criterion,
                        Plan,
                        The_Assignment,
                        null);
   end Assign_Best_Plan;

   ----------------------
   -- Assign_Best_Plan --
   ----------------------

   procedure Assign_Best_Plan
     (The_Assigner   : in     Object'Class;
      Agents         : in     Agent.Lists.List;
      Plans          : in     Htn.Plan.Object; -- OR of possible plans.
      Criterion      : in     Assignment_Criteria;
      Plan           :    out Htn.Plan.Object; -- Selected plan with best cost.
      The_Assignment :    out Assignment.Object;
      Enumerate      : access procedure (A : in Assignment.Object))
   is
      use Htn.Plan;
      Best_Cost : Costs := Costs'Last;
   begin
      pragma Assert (Htn.Plan_Node.Get_Kind (Get_Root (Plans)) = Htn.Plan_Node.Or_Node);

      declare
         use Htn.Plan_Node.Node_Lists;
         Nodes : constant List   := Htn.Plan_Node.Get_Children (Get_Root (Plans));
         I     :          Cursor := Nodes.First;
      begin
         while Has_Element (I) loop
            declare
               Tasks : Htn.Tasks.Lists.List;
               Assig : Assignment.Object;
               Costs : Cost_Matrix.Object;

               use Assignment;
            begin
               Htn.Plan_Node.Enumerate_Tasks (Element (I), Tasks,
                                              Primitive => True,
                                              Pending   => True);

               --  Create cost matrix to be used
               Cost_Matrix.Create_With_Start (Costs, Agents, Tasks);

               --  Assign this plan and see cost
               Assig := The_Assigner.Assign (Agents, Tasks, Costs);

               if Enumerate /= null then
                  Enumerate (Assig);
               end if;

--               Put_Line ("Plan_Cost:" & Get_Cost (Assig, Criterion)'Img);

               if Get_Cost (Assig, Criterion) < Best_Cost then
                  Best_Cost      := Get_Cost (Assig, Criterion);
                  The_Assignment := Assig;
                  Plan           := Copy_But_Tasks (Plans);
                  Add_Subplan (Plan,
                               Htn.Plan_Node.Deep_Copy (Htn.Plan_Node.Node_Lists.Element (I)));
               end if;
            end;
            Next (I);
         end loop;
      end;
   end Assign_Best_Plan;

end Agpl.Cr.Assigner;
