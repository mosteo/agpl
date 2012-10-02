

--  This package provides smart access pointers.
--  Thread *safe*.

with Agpl.Smart_Access_Limited;
pragma Elaborate_All (Agpl.Smart_Access_Limited);

with Ada.Streams;

generic
   type Item (<>)   is private;         -- Type.
   type Item_access is access Item;     -- This is the access we want safe.
   Item_id : String := "Anonymous";     -- For debug and error reporting.
   Debug   : Boolean := False;          -- Log bind/free
package Agpl.Smart_Access is

   pragma Preelaborate;

   package Lim is new Smart_Access_Limited (Item, Item_Access, Item_Id, Debug);

   type Object is new Lim.Object with private;

   --  Get value
   function Val (This : in Object) return Item;
   function "+" (This : in Object) return Item renames Val;

   --  Serialization...
   function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object;
   for Object'Input use Input;

   procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
                     This : in Object);
   for Object'Output use Output;

private

   pragma Inline (Val);

   type Object is new Lim.Object with null record;

end Agpl.Smart_access;
