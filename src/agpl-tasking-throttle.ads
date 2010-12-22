package Agpl.Tasking.Throttle is

   Log_Section : constant String := "agpl.tasking.throttle";

   type Object is abstract tagged private;

   type Budgets is range 1 .. 100; -- Percent of CPU allowed
   --  By default, this object has 100% budget.

   procedure Code (This : in out Object) is abstract;
   --  Override with what has to be throttled.

   procedure Run (This : in out Object);
   --  Call this to obtain throttled executions of Code.
   --  This may block for some time to achieve throttling

   procedure Set_Budget (This   : in out Object;
                         Budget :        Budgets);

private

   type Object is abstract tagged record
      Used   : Duration := 0.0;
      Budget : Budgets  := 100;
   end record;

end Agpl.Tasking.Throttle;
