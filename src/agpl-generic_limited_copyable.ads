with Ada.Finalization;

generic
   type Non_Copyable is limited private;
   type Value is private;
   with function Get (From : Non_Copyable) return Value;
   --  Must return some nonlimited representation of Non_Copyable
   with procedure Set (This : in out Non_Copyable; Val : Value);
   --  Must set This to the value given
package Agpl.Generic_Limited_Copyable is

   type Object is new Ada.Finalization.Controlled with private;
   --  This new type makes deep copies of its Limited counterpart

   not overriding
   procedure Set (This : Object; Val : Value);

private

   overriding
   procedure Adjust (This : in out Object);

end Agpl.Generic_Limited_Copyable;
