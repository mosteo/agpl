--  with Agpl.Text_Io; use Agpl.Text_Io;

package body Agpl.Tasking.Period is

   use Ada.Calendar;

   ------------
   -- Create --
   ------------

   function Create (Period : Duration) return Object is
   begin
      return Object'(First_Time => True,
                     Period     => Period,
                     Now        => Ada.Calendar.Clock);
   end Create;

   ----------------
   -- Get_Period --
   ----------------

   function Get_Period (This : Object) return Duration is
   begin
      return This.Period;
   end Get_Period;

   ----------------
   -- Set_Period --
   ----------------

   procedure Set_Period
     (This   : in out Object;
      Period :        Duration)
   is
   begin
      This.Period := Period;
   end Set_Period;

   ----------
   -- Next --
   ----------

   procedure Next
     (This     : in out Object;
      Time     :    out Ada.Calendar.Time;
      Catch_Up :        Boolean := Default_Catch_Up)
   is
      pragma Assert (This.Period > 0.0, "Uninitialized period");
   begin
      if This.First_Time then
         This.Now        := Clock + This.Period;
         This.First_Time := False;
         Time            := This.Now;
      else
         --  Catch-up included if demanded.
         loop
            This.Now := This.Now + This.Period;
            exit when (not Catch_Up) or else This.Now > Clock;
         end loop;

         --  Final catch-up for next call, if we're missing it now!
         if (not Catch_Up) and then This.Now < Clock then
            while This.Now < Clock loop
               This.Now := This.Now + This.Period;
               --            Put_Line (".");
            end loop;
         end if;

         Time := This.Now;
      end if;
   end Next;

end Agpl.Tasking.Period;
