with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists,
     Ada.Finalization,
     Ada.Unchecked_Deallocation;
with Agpl.Average_Queue;
with Agpl.Average_Queue.Timed;
with Agpl.Conversions;
with Agpl.Counter.Multi,
     Agpl.Generic_Handle,
     Agpl.Trace,
     Agpl.Types.Ustrings;

use Agpl.Trace,
    Agpl.Types.Ustrings;

package body Agpl.Tasking.Generic_Workers is

   subtype Rates is Float;

   package Avg_Rates_Base is new Agpl.Average_Queue (Rates);
   package Avg_Rates      is new Avg_Rates_Base.Timed;

   Class_Counter : Agpl.Counter.Multi.Object;

   Thread_Ini_Rate : Avg_Rates.Object (Slots => 5, Slot_Duration => 1000);
   Thread_Fin_Rate : Avg_Rates.Object (Slots => 5, Slot_Duration => 1000);

   package Code_Handles is new Agpl.Generic_Handle (Code);
   subtype Code_Handle is Code_Handles.Object;
   use Code_Handles;

   ------------
   -- Worker --
   ------------

   task type Worker (Stack_Size : Natural) is
      pragma Storage_Size (Stack_Size);

      entry Set_Class (Class : String);
      entry Launch (This : Code);
   end Worker;

   type Worker_Access is access Worker;

   procedure Free is new Ada.Unchecked_Deallocation (Worker, Worker_Access);

   package Worker_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Worker_Access);

   -------------
   -- Aborter --
   -------------

   task type Aborter (W : Worker_Access);

   task body Aborter is
   begin
      abort W.all;
   end Aborter;

   type Jobs is record
      Work  : Code_Handle;
      Class : Ustring;
      Stack : Natural;
   end record;

   package Job_Lists is new Ada.Containers.Doubly_Linked_Lists (Jobs);

   ----------
   -- Safe --
   ----------

   protected Safe is
      procedure Increment;
      procedure Decrement;

      procedure Add_Job (Job : Jobs);
      entry Get_Job (Job : out Jobs);
      --  remove first pending job

      procedure Abort_All;
      procedure Append (W : Worker_Access);
      procedure Check_Old;
      function  Count return Natural;
   private
      Counter         : Natural := 0;
      Running_Workers : Worker_Lists.List;
      Pending_Jobs    : Job_Lists.List;
   end Safe;

   ----------
   -- Safe --
   ----------

   protected body Safe is

      ------------
      -- Append --
      ------------

      procedure Append (W : Worker_Access) is
      begin
         Running_Workers.Append (W);
      end Append;

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

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Counter;
      end Count;

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement is
      begin
         Counter := Counter - 1;
      end Decrement;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Counter := Counter + 1;
      end Increment;

      ---------------
      -- Abort_All --
      ---------------

      procedure Abort_All is
         procedure Abort_Worker (I : Worker_Lists.Cursor) is
            A : constant access Aborter :=
                  new Aborter (Worker_Lists.Element (I));
            pragma Unreferenced (A);
         begin
            null;
         end Abort_Worker;
      begin
         Running_Workers.Iterate (Abort_Worker'Access);
      end Abort_All;

      -------------
      -- Add_Job --
      -------------

      procedure Add_Job (Job : Jobs) is
      begin
         Pending_Jobs.Append (Job);
      end Add_Job;

      -------------
      -- Get_Job --
      -------------

      entry Get_Job (Job : out Jobs)
        when not Pending_Jobs.Is_Empty is
      begin
         Job := Pending_Jobs.First_Element;
         Pending_Jobs.Delete_First;
      end Get_Job;

   end Safe;

   ----------
   -- Live --
   ----------

   task Live;

   task body Live is
   begin
      loop
         declare
            Job : Jobs;
         begin
            Safe.Get_Job (Job);
            Launch (Job.Work.Ref.all,
                    +Job.Class,
                    Job.Stack,
                    Reap_Old => False,
                    Activate => True);
         exception
            when E : others =>
               Log ("Agpl.Tasking.Workers.Live: " & Report (E),
                    Error, Log_Section);
         end;
      end loop;
   end Live;

   type Autocounter is new Ada.Finalization.Limited_Controlled with null record;
   procedure Initialize (This : in out Autocounter);
   procedure Finalize   (This : in out Autocounter);

   procedure Initialize (This : in out Autocounter) is
      pragma Unreferenced (This);
   begin
      Safe.Increment;
   end Initialize;

   procedure Finalize   (This : in out Autocounter) is
      pragma Unreferenced (This);
   begin
      Safe.Decrement;
   end Finalize;

   ---------------
   -- Abort_All --
   ---------------

   procedure Abort_All is
   begin
      Safe.Abort_All;
   end Abort_All;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return Safe.Count;
   end Count;

   ------------
   -- Worker --
   ------------

   task body Worker is
      C : Code_Handle;
      A : Autocounter; pragma Unreferenced (A);

      subtype Code_Class is Agpl.Tasking.Code.Object'Class;

      Class : Ustring;
   begin
      Thread_Ini_Rate.Push (1.0);

      accept Set_Class (Class : String) do
         Worker.Class := +Class;
      end Set_Class;

      Class_Counter.Add (+Class);

      accept Launch (This : Code) do
         C.Set (This);
      end Launch;

      begin
         C.Ref.all.Init;
      exception
         when E : others =>
            Log ("Agpl.Tasking.Worker [init]: " &
                 External_Tag (Code_Class (C.Ref.all)'Tag) & ": " & Report (E),
                 Error);
      end;

      begin
         C.Ref.all.Run;
      exception
         when E : others =>
            Log ("Agpl.Tasking.Worker [run]: " &
                 External_Tag (Code_Class (C.Ref.all)'Tag) & ": " & Report (E),
                 Error);
      end;

      begin
         C.Ref.all.Destroy;
      exception
         when E : others =>
            Log ("Agpl.Tasking.Worker [destroy]: " &
                 External_Tag (Code_Class (C.Ref.all)'Tag) & ": " & Report (E),
                 Error);
      end;

      Class_Counter.Add (+Class, -1);
      Thread_Fin_Rate.Push (1.0);

   exception
      when E : others =>
         Class_Counter.Add (+Class, -1);
         Thread_Fin_Rate.Push (1.0);

         Log ("Agpl.Tasking.Worker: " & Report (E), Error);
         raise;
   end Worker;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (This     : Code;
      Class    : String  := "";
      Stack    : Natural := 64 * 1024;
      Reap_Old : Boolean := True;
      Activate : Boolean := False)
   is
   begin
      if Activate then
         declare
            W : constant Worker_Access := new Worker (Stack);
         begin
            W.Set_Class (Class);
            W.Launch (This);

            Safe.Append (W);
         end;
      else
         Safe.Add_Job ((Work  => Set (This),
                        Class => +Class,
                        Stack => Stack));
      end if;

      if Reap_Old then
         Safe.Check_Old;
      end if;
   end Launch;

   ---------------
   -- Purge_Old --
   ---------------

   procedure Purge_Old is
   begin
      Safe.Check_Old;
   end Purge_Old;

   -----------------
   -- Class_Count --
   -----------------

   function Class_Count (Class : String) return Natural is
   begin
      return Class_Counter.Val (Class);
   end Class_Count;

   -------------------
   -- Rate_Informer --
   -------------------

   task Rate_Informer is
      entry Avg_Ini (Avg : out Float);
      entry Avg_Fin (Avg : out Float);
   end Rate_Informer;

   task body Rate_Informer is
      use Ada.Calendar;
      use Agpl.Conversions;
      Next    : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      loop
         declare
            Av_Ini : Rates;
            Av_Fin : Rates;
         begin
            thread_ini_Rate.Average (Av_Ini);
            Thread_Fin_Rate.Average (Av_Fin);

            Log ("New threads: " & To_String (Float (Av_Ini), Decimals => 1) &
                 " (" & To_String (Float (Av_Ini), Decimals => 1) & "/s)",
                 Informative, Log_Section_Rates);
            Log ("Fin threads: " & To_String (Float (Av_Fin), Decimals => 1) &
                 " (" & To_String (Float (Av_Fin), Decimals => 1) & "/s)",
                 Informative, Log_Section_Rates);

            Next := Next + 1.0;

            while Next > Clock loop
               select
                  accept Avg_Ini (Avg : out Float) do
                     Avg := Av_Ini;
                  end Avg_Ini;
               or
                  accept Avg_Fin (Avg : out Float) do
                     Avg := Av_Fin;
                  end Avg_Fin;
               or
                  delay until Next;
               end select;
            end loop;
         end;
      end loop;
   end Rate_Informer;

      ---------------------
      -- Avg_Threads_New --
      ---------------------

      function Avg_Threads_New return Float is
         X : Float;
      begin
         Rate_Informer.Avg_Ini (X);
         return X;
      end Avg_Threads_New;

      ---------------------
      -- Avg_Threads_End --
      ---------------------

      function Avg_Threads_End return Float is
         X : Float;
      begin
         Rate_Informer.Avg_Fin (X);
         return X;
      end Avg_Threads_End;

end Agpl.Tasking.Generic_Workers;
