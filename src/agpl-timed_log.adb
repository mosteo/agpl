package body Agpl.Timed_Log is

   ------------
   -- Create --
   ------------

   function Create
     (Text   : String;
      Level  : Trace.Levels := Trace.Always;
      Scoped : Boolean      := True)
      return Object
   is
   begin
      Trace.Log (Text & "...", Level);
      return R : Object do
         R.Text   := +Text;
         R.Level  := Level;
         R.Scoped := Scoped;
      end return;
   end Create;

   --------------
   -- Snapshot --
   --------------

   procedure Snapshot (This  : Object;
                       Step  : String := "BUSY";
                       Level : Trace.Levels := Trace.Always) is
   begin
      Trace.Log ((+This.Text) & "... " & Step &
                 " [" & This.Timer.Image & "]",
                 Level);
   end Snapshot;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      if This.Scoped then
         Trace.Log ((+This.Text) & "... DONE [" & This.Timer.Image & "]",
                    This.Level);
      end if;
   end Finalize;

end Agpl.Timed_Log;
