with Htn_Test;

with Agpl.Debug;
with Agpl.Htn.Method;
with Agpl.Htn.Plan;
use Agpl.Htn;

with Gnat.Debug_Pools;

with Text_Io;

procedure Htn1 is
   procedure Get_Plan (This : in Plan.Object) is
      use Text_Io;
   begin
      Put_Line ("Valid plan found:");
      Plan.Print_Summary (This);
   end Get_Plan;
begin
   declare
      P  : Plan.Object;
      T3 : Htn_Test.Task_Split (Value => 3);
      T2 : Htn_Test.Task_Split (Value => 2);
      M  : Htn_Test.Method_Split;
   begin

      Plan.Add_Task (P, T3);
      Plan.Add_Task (P, T2);
      Plan.Add_Method (P, M);
      Plan.Print_Summary (P);
      Plan.Expand (P, Get_Plan'Unrestricted_Access);
   end;

   Gnat.Debug_Pools.Print_Info_Stdout (Agpl.Debug.Pool);
end Htn1;
