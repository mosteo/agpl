

package body Agpl.Monitor is

   protected body Counting_Semaphore is

      entry P when True is
      --  Not sure if this can ever work...
      --  a Potentially Blocking will happen?
      begin
         if Caller = Counting_Semaphore.P'Caller then
            In_Use := In_Use + 1;
         elsif In_Use = 0 then
            Caller := Counting_Semaphore.P'Caller;
            In_Use := 1;
         else
            requeue Safe_P with abort;
         end if;
      end P;

      entry Safe_P when In_Use = 0 is
      begin
         Caller := Safe_P'Caller;
         In_Use := 1;
      end Safe_P;

      entry V when True is
      begin
         if Counting_Semaphore.V'Caller /= Caller then
            raise Use_Error;
         else
            In_Use := In_Use - 1;
            if In_Use = 0 then
               Caller := Null_Task_Id;
            end if;
         end if;
      end V;

   end Counting_Semaphore;

   protected body Fake_Semaphore is
      procedure P is
      begin
         null;
      end P;

      procedure V is
      begin
         null;
      end V;
   end Fake_Semaphore;

   ----------------
   -- Initialize --
   ----------------
   --  Get
   procedure Initialize (This : in out Object) is
   begin
      if This.S /= null then
         This.S.P;
      end if;
   end Initialize;


   --------------
   -- Finalize --
   --------------
   --  Release
   procedure Finalize (This : in out Object) is
   begin
      if This.S /= null then
         This.S.V;
      end if;
   end Finalize;

end Agpl.Monitor;
