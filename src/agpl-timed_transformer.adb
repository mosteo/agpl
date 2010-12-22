package body Agpl.Timed_Transformer is

   ------------
   -- Create --
   ------------

   function Create (Val : Real) return Object is
   begin
      return (Val => Val,
              Old => Val,
              others => <>);
   end Create;

   ---------
   -- Set --
   ---------

   procedure Set
     (This : in out Object;
      Val    :        Real;
      Period :        Duration)
   is
   begin
      This.Old    := This.Value; -- Use current, to be smooth.
      This.Val    := Val;
      This.Period := Period;
      This.Timer.Reset;
   end Set;

   -----------
   -- Value --
   -----------

   function Value (This : Object) return Real is
      Elapsed : constant Duration := This.Timer.Elapsed;
   begin
      if Elapsed >= This.Period then
         return This.Val;
      else
         return This.Old + (This.Val - This.Old) * Real (Elapsed / This.Period);
      end if;
   end Value;

end Agpl.Timed_Transformer;
