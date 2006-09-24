with Agpl.Cr.Containers; use Agpl.Cr.Containers;

package body Agpl.Cr.Assignment.Utils is

   -----------------
   -- Concatenate --
   -----------------

   procedure Concatenate (Dst : in out Object; Src : Object) is
      Agents : constant Agent_Lists.List := Src.Get_Agents;

      procedure Do_It (I : Agent_Lists.Cursor) is
         Ag_Src : constant Cr.Agent.Object'Class := Agent_Lists.Element (I);
      begin
         if Dst.Contains (Ag_Src.Get_Name) then
            declare
               Ag_Dst : Cr.Agent.Object'Class := Dst.Get_Agent (Ag_Src.Get_Name);
               T      : Task_Lists.List       := Ag_Dst.Get_Tasks;
            begin
               Task_Utils.Concatenate (T, Ag_Src.Get_Tasks);
               Ag_Dst.Set_Tasks (T);
               Dst.Set_Agent (Ag_Dst);
            end;
         else
            Dst.Set_Agent (Ag_Src);
         end if;
      end Do_It;

   begin
      Agents.Iterate (Do_It'Access);
   end Concatenate;

end Agpl.Cr.Assignment.Utils;
