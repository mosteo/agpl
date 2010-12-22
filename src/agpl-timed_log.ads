with Ada.Finalization;
with Agpl.Chronos;
with Agpl.Trace;
with Agpl.Ustrings;

package Agpl.Timed_Log is

   type Object (<>) is tagged limited private;

   --  Logs the given text

   function Create (Text   : String;
                    Level  : Trace.Levels := Trace.Always;
                    Scoped : Boolean      := True) return Object;

   procedure Snapshot (This  : Object;
                       Step  : String := "BUSY";
                       Level : Trace.Levels := Trace.Always);

private

   use Agpl.Ustrings;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Timer  : Chronos.Object;
      Text   : Ustring;
      Scoped : Boolean;
      Level  : Trace.Levels;
   end record;

   procedure Finalize (This : in out Object);

end Agpl.Timed_Log;
