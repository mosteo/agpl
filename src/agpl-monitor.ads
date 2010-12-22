

--  Mutex with counter. A task may safely request it multiple times,
--  as long as it releases it the same times

with Ada.Finalization;
with Ada.Task_Identification;

package Agpl.Monitor is

   pragma Preelaborate;

   use Ada;
   use Ada.Task_Identification;

   Use_Error : exception; -- Should never happen.

   type Semaphore is synchronized interface;

   procedure P (This : in out Semaphore) is abstract;
   procedure V (This : in out Semaphore) is abstract;

   --  Standard Counting semaphore:
   protected type Counting_Semaphore is new Semaphore with
      entry P;
      entry V;
   private
      entry Safe_P;

      Caller : Task_Id := Null_Task_Id;           -- Requester
      In_Use : Natural := 0;                      -- Times requested
   end Counting_Semaphore;

   --  Fake for testing:
   protected type Fake_Semaphore is new Semaphore with
      procedure P;
      procedure V;
   end Fake_Semaphore;

   type Semaphore_Access is access all Semaphore'Class;

   --  The following object is defined for conveniently usage of semaphores.
   --  Use:
   --  S : aliased Semaphore;
   --  declare
   --    M : Object (S'access);
   --  begin
   --    Exclusive_work;
   --  end;
   type Object (S : access Semaphore'Class) is new
     Finalization.Limited_Controlled with null record;
   --  Note that for generality purposes, if S in null no error will happen

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

private

   pragma Inline (Initialize, Finalize);

end Agpl.Monitor;
