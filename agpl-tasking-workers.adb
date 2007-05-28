with Agpl.Generic_Handle;
with Agpl.Trace;          use Agpl.Trace;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

package body Agpl.Tasking.Workers is

   package Code_Handle is new Agpl.Generic_Handle (Code'Class);

   task type Worker (Stack_Size : Natural) is
      pragma Storage_Size (Stack_Size);

      entry Launch (This : Code'Class);
   end Worker;

   type Worker_Access is access Worker;

   procedure Free is new Ada.Unchecked_Deallocation (Worker, Worker_Access);

   package Worker_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Worker_Access);

   Running_Workers : Worker_Lists.List;

   ------------
   -- Worker --
   ------------

   task body Worker is
      C : Code_Handle.Object;
   begin
      accept Launch (This : Code'Class) do
         C.Set (This);
      end Launch;

      C.Ref.all.Run;
   exception
      when E : others =>
         Log ("Agpl.Tasking.Worker: " & Report (E), Error);
   end Worker;

   ---------------
   -- Check_Old --
   ---------------

   procedure Check_Old is
      use Worker_Lists;
      I : Cursor := Worker_Lists.Last (Running_Workers);
      J : Cursor;
   begin
      while Has_Element (I) loop
         J := Previous (I);

         declare
            W : Worker_Access := Element (I);
         begin
            if W'Terminated then
               Free (W);
               Running_Workers.Delete (I);
            end if;
         end;

         I := J;
      end loop;
   end Check_Old;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (This     : Code'Class;
      Stack    : Natural := 64 * 1024;
      Reap_Old : Boolean := True)
   is
      W : constant Worker_Access := new Worker (Stack);
   begin
      W.Launch (This);

      Running_Workers.Append (W);

      if Reap_Old then
         Check_Old;
      end if;
   end Launch;

end Agpl.Tasking.Workers;
