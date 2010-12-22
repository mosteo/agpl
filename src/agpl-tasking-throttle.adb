with Agpl.Chronos;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Tasking.Throttle is

   Base_Slice : constant Duration := 0.05;
   --  We don't throttle until this much execution time has been acummulated.

   Max_Sleep : constant Duration := 0.5;
   --  Won't sleep for more that this in any case to prevent mishaps

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object) is
      Timer : Chronos.Object;
   begin
      Object'Class (This).Code;

      This.Used := This.Used + Timer.Elapsed;
      --  This is just approximative, since we can be preempted or blocked...

      if This.Used >= Base_Slice then
         declare
            Budget : constant Duration := Duration (This.Budget) / 100.0;
            Factor : constant Duration := 1.0 / Budget - 1.0;
            pragma Assert (Factor >= 0.0);
         begin
            Log ("Sleeping" & Duration'Min (Max_Sleep, This.Used * Factor)'Img &
                 " seconds...", Debug, Log_Section);
            delay Duration'Min (Max_Sleep, This.Used * Factor);

            This.Used := 0.0;
         end;
      end if;
   end Run;

   ----------------
   -- Set_Budget --
   ----------------

   procedure Set_Budget
     (This   : in out Object;
      Budget :        Budgets)
   is
   begin
      This.Budget := Budget;
   end Set_Budget;

end Agpl.Tasking.Throttle;
