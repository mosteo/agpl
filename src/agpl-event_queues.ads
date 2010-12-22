

--  Efficient event queue. Useful for timeouts, as an example.

--  Implemented with tagged types. That makes genericity unnecesary. A queue
--   can perform multiple kind of events.

package Agpl.Event_queues is

   pragma Preelaborate;

   --  This type can be extended to match any required context
   type Context_type is tagged null record;

   --  Callback function called when event triggers
   type Action_Procedure is access procedure (Context : Context_Type'Class);

   type Master_States is (Waiting_Worker, Waiting_Deadline, Executing, Idle, Ready);
   type Worker_States is (Waiting, Executing);

private

   type Action_type is (New_event, Job_finished);

   type Context_Access is access all Context_Type'Class;
   --   for Context_access'Storage_pool use Adagio.Debug_pool;

end Agpl.Event_queues;
