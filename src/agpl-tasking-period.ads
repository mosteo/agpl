with Ada.Calendar;

package Agpl.Tasking.Period is

   --  Soft real-time non-skewing periods

   Default_Catch_Up : Boolean := False;
   --  This governs global behavior, not very clever but maybe useful for debug

   type Object is tagged private;

   function Create (Period : Duration) return Object;

   function Get_Period (This : Object) return Duration;

   procedure Set_Period (This   : in out Object;
                         Period :        Duration);

   procedure Next (This     : in out Object;
                   Time     :    out Ada.Calendar.Time;
                   Catch_Up :        Boolean := Default_Catch_Up);
   --  Move to next slot and give it
   --  If Catch_Up, Next is guaranteed to be in the future even if this means
   --   skipping some slots
   --  If not Catch_Up, we may get a time in the past, but the internal slots
   --   are moved forward nonetheless so your next call should be again in the
   --   future.
   --  This prevents some saturation in certain cases.

private

   type Object is tagged record
      First_Time : Boolean  := True;
      Now        : Ada.Calendar.Time;
      Period     : Duration := -1.0; -- To detect uninitialization
   end record;

end Agpl.Tasking.Period;
