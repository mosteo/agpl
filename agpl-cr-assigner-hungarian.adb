with Agpl.Cr.Agent.Utils; use Agpl.Cr.Agent.Utils;
with Agpl.Htn.Tasks.Utils; use Agpl.Htn.Tasks.Utils;

with Hungarian;

package body Agpl.Cr.Assigner.Hungarian is

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Agpl.Htn.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return Assignment.Object
   is
      pragma Unreferenced (This);

      package Hu is new Standard.Hungarian.Solver (Natural (Agents.Length),
                                                   Natural (Tasks.Length));

      A : Ac.Vectors.Vector := +Agents;
      T : Tc.Vectors.Vector := +Tasks;
      H : Hu.Problem;
      C : Hu.Cost_Matrix;
   begin
      if A.Is_Empty or else T.Is_Empty then
         return Cr.Assignment.Empty_Object;
      end if;

      --  Create hungarian problem
      for I in C'Range (1) loop
         for J in C'Range (2) loop
            C (I, J) := Hu.Costs
              (Costs.Get_Cost (A.Element (Natural (I)).Get_Name,
               Htn.Tasks.No_Task,
               T.Element (Natural (J)).Get_Id));
         end loop;
      end loop;
      H.Create (C);
      H.Solve;

      --  Solve and convert back to assignment
      declare
         S : constant Hu.Solution_Array := H.Solution;
         R :          Assignment.Object;
      begin
         for I in A.First_Index .. A.Last_Index loop
            if S (Hu.Worker_Index (I)) in 1 .. Hu.Job_Index (T.Length) then
               R.Set_Agent (A.Element (I));
               R.Add       (A.Element (I),
                            T.Element (Natural (S (Hu.Worker_Index (I)))));
            end if;
         end loop;
         return R;
      end;
   end Assign;

   ------------
   -- Assign --
   ------------

   function Assign
     (Agents : in Agent.Containers.Lists.List;
      Tasks  : in Agpl.Htn.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return      Assignment.Object
   is
      Dummy : constant Object := (Assigner.Object with null record);
   begin
      return Dummy.Assign (Agents, Tasks, Costs);
   end Assign;

end Agpl.Cr.Assigner.Hungarian;
