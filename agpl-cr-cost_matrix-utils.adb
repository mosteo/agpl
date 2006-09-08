with Agpl.Htn.Tasks.Utils;

package body Agpl.Cr.Cost_Matrix.Utils is

   package Ac renames Agpl.Cr.Agent.Containers;
   package Tc renames Agpl.Htn.Tasks.Containers;

   -----------
   -- Prune --
   -----------

   procedure Prune
     (Dst :    out Cost_Matrix.Object;
      Src : in     Cost_Matrix.Object;
      Ass : in     Assignment.Object)
   is
      use Ac.Lists;
      use Htn.Tasks.Utils;

      procedure Check_Agent (I : Cr.Agent.Containers.Lists.Cursor) is
         Agent : constant Cr.Agent.Object'Class := Element (I);
         V     : Tc.Vectors.Vector := To_Vector (Ass.Get_Tasks (Agent));
      begin
         --  Starting tasks
         if not V.Is_Empty then
            Set_Cost
              (Dst, Agent.Get_Name,
               Htn.Tasks.No_Task,
               V.First_Element.Get_Id,
               Get_Cost
                 (Src, Agent.Get_Name,
                  Htn.Tasks.No_Task,
                  V.First_Element.Get_Id));
         end if;
         --  Used tasks
         for I in V.First_Index .. V.Last_Index - 1 loop
            Set_Cost
              (Dst, Agent.Get_Name,
               V.Element (I).Get_Id,
               V.Element (I + 1).Get_Id,
               Get_Cost
                 (Src, Agent.Get_Name,
                  V.Element (I).Get_Id,
                  V.Element (I + 1).Get_Id));
         end loop;
      end Check_Agent;
   begin
      Ass.Get_Agents.Iterate (Check_Agent'Access);
   end Prune;

end Agpl.Cr.Cost_Matrix.Utils;
