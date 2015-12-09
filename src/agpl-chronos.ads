

--  Chronometer. Starts counting on creation.
--  THREAD SAFE

with Ada.Calendar;
--  with Agpl.Smart_access_Limited;
--  pragma TO_DO ("Migrate to Ada.Real_Time");

package Agpl.Chronos is

--     pragma Elaborate_Body;

   type Object is tagged private;
   type Object_Access is access Object'Class;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Elapsed : Duration := 0.0);
   pragma Inline (Reset);
   function Reset (Elapsed : Duration) return Object;

   ------------------------------------------------------------------------
   -- Elapsed                                                            --
   ------------------------------------------------------------------------
   function Elapsed (This : in Object) return Duration;
   pragma Inline (Elapsed);

   ------------------------------------------------------------------------
   -- Image                                                              --
   ------------------------------------------------------------------------
   function Image (This : in Object) return String;

   function Value (This : in Object) return Ada.Calendar.Time;
   pragma Inline (Value);
   --  This start time of this object

   function Clock return Object; -- An object denoting current time
   pragma Inline (Clock);

   function Epoch return Object; -- An object denoting start of Ada time
   pragma Inline (Epoch);

private

--     protected type Safe is
--        procedure Set (To_Time : Ada.Calendar.Time := Ada.Calendar.Clock);
--        function  Get return Ada.Calendar.Time;
--     private
--        Start : Ada.Calendar.Time := Ada.Calendar.Clock;
--     end Safe;
--
--     type Safe_Access is access all Safe;
--
--     Package Smart_Safes is new Agpl.Smart_Access_Limited (Safe, Safe_Access);

   type Object is tagged record
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
     --  pragma Atomic (Start);
   end record;

end Agpl.Chronos;
