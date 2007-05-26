package Agpl.Tasking.Workers is

   pragma Preelaborate;

   type Code is interface;
   --  Non-limited because a copy is needed later

   procedure Run (This : in out Code) is abstract;
   --  override Run with your desired code to be executed in a new task

   procedure Launch (This     : Code'Class;
                     Stack    : Natural := 64 * 1024;
                     Reap_Old : Boolean := True);
   --  A copy will be made of This for internal use.
   --  If Reap_Old, then after launching the new worker, an attemp will be
   --  made at freeing old finished workers.

end Agpl.Tasking.Workers;
