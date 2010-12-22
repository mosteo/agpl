with Agpl.Tasking.Code;

generic
   --  Made generic just so other generics can instantiate local pools,
   --  Otherwise, accesibility checks won't allow this to work.
   type Code (<>) is new Agpl.Tasking.Code.Object with private;
   with function "=" (L, R : Code) return Boolean is <>;
package Agpl.Tasking.Generic_Workers is

--     pragma Preelaborate;

   Log_Section       : constant String := "agpl.tasking.workers";
   Log_Section_Rates : constant String := "agpl.tasking.workers.rates";

   procedure Launch (This     : Code;
                     Class    : String  := "";
                     Stack    : Natural := 64 * 1024;
                     Reap_Old : Boolean := True;
                     Activate : Boolean := False);
   --  A copy will be made of This for internal use.
   --  If Reap_Old, then after launching the new worker, an attemp will be
   --  made at freeing old finished workers.
   --  Class is for grouping counts only
   --  If Activate, a task is immediately created; in this case:
   --  NOTE: this is a potentially blocking operation, since the worker is
   --  immediately created
   --  If not activate, it isn't potentially blocking.

   function Count return Natural;
   --  O (1)

   function Class_Count (Class : String) return Natural;
   --  O (log n)

   procedure Purge_Old;
   --  O (n)
   --  Free memory for old finished workers.

   procedure Abort_All;
   --  O (n)
   --  Will attempt to kill every running worker...
   --  No guarantee on this working really...
   --  Resources can be leaked all around the place.
   --  For sure, some memory will be leaked in the aborter jobs launched.

   function Avg_Threads_New return Float;
   function Avg_Threads_End return Float;

end Agpl.Tasking.Generic_Workers;
