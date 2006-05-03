with Htn_Test;

with Agpl.Debug;
with Agpl.Htn.Plan;
use Agpl.Htn;

with Gnat.Debug_Pools;

with Text_Io;

--  Intended to test expansion of tasks with compound -> compound -> primitive
--  complexity.

procedure Htn3 is
   procedure Get_Plan (This : in Plan.Object) is
      use Text_Io;
   begin
      Put_Line ("Valid plan found:");
      Plan.Print_Summary (This);
   end Get_Plan;
begin
   declare
      P  : Plan.Object;
      T  : Htn_Test.Task_Divide (Value => 4);
      M  : Htn_Test.Method_Split;
   begin

      Plan.Add_Task (P, T);
      Plan.Add_Method (P, M);
      Plan.Print_Summary (P);
      Plan.Expand (P, Get_Plan'Unrestricted_Access);
   end;

   Gnat.Debug_Pools.Print_Info_Stdout (Agpl.Debug.Pool);
end Htn3;
