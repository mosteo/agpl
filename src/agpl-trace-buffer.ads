with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Agpl.Ustrings; use Agpl.Ustrings;
with Agpl.Trace.Root;

package Agpl.Trace.Buffer is

   type Object (Max_Length : Natural) is new Root.Object with private;
   --  Stores at most Max_Length log messages

   type Object_Access is access all Object'Class;

   pragma Preelaborable_Initialization (Object);

   type Log_Entry is record
      Level : Levels;
      Text  : Ustring;
      Sect  : Ustring;
      Stamp : Ada.Calendar.Time;
   end record;

   overriding
   procedure Log (This    : in out Object;
                  Text    : in String;
                  Level   : in Levels;
                  Section : in String := "");

   package Entry_Lists is new Ada.Containers.Doubly_Linked_Lists (Log_Entry);

   not overriding
   function Get (This : Object) return Entry_Lists.List;

private

   protected type Safe (Max_Length : Natural) is

      procedure Add (X : Log_Entry);

      function Get return Entry_Lists.List;

   private

      Entries : Entry_Lists.List;

   end Safe;

   type Object (Max_Length : Natural) is new Root.Object with record
      Entries : Safe (Max_Length);
   end record;

end Agpl.Trace.Buffer;
