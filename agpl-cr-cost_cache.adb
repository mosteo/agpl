with Agpl.Cr.Agent.Dummy;
--  with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Cost_Cache is

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in Cr.Agent.Object'Class)
      return Costs
   is
      T    : constant Htn.Tasks.Containers.Lists.List := Agent.Get_Tasks;
      Prev :          Htn.Tasks.Task_Id               := Htn.Tasks.No_Task;
      use Htn.Tasks.Containers.Lists;

      Total,
      Partial : Cr.Costs                          := 0.0;
      I       : Htn.Tasks.Containers.Lists.Cursor := T.First;
   begin
      while Has_Element (I) loop
         Partial := Get_Cost (This,
                              Cr.Agent.Get_Name (Agent),
                              Prev, Htn.Tasks.Get_Id (Element (I)));

         if Partial = Cr.Infinite then
            Total := Infinite;
         else
            Total := Total + Partial;
         end if;
         exit when Partial = Cr.Infinite;
         Prev := Htn.Tasks.Get_Id (Element (I));
         Next (I);
      end loop;
      return Total;
   end Get_Plan_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in String;
      Tasks : in Htn.Tasks.Containers.Lists.List)
      return Costs
   is
      Ag : Cr.Agent.Dummy.Object;
   begin
      Ag.Set_Name (Agent);
      Ag.Set_Tasks (Tasks);
      return Get_Plan_Cost (This, Ag);
   end Get_Plan_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This  : in Empty_Class;
      Agent : in String;
      Ini   : in Htn.Tasks.Task_Id;
      Fin   : in Htn.Tasks.Task_Id) return Costs
   is
      pragma Unreferenced (This, Agent, Ini, Fin);
   begin
      return Cr.Infinite;
   end Get_Cost;

end Agpl.Cr.Cost_Cache;
