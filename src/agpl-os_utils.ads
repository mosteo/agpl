with Agpl.Containers.String_Vectors;

package Agpl.Os_Utils is

   function Get_Free_Disk_Space (Path : String := ".") return Float;

   function Spawn (Command : String) return Integer;
   --  Launch a command, return its exit code (or -1 if couldn't spawn it)

   procedure Mtrace;
   pragma Import (C, Mtrace, "agpl__os_utils__mtrace");

   procedure Muntrace;
   pragma Import (C, Muntrace, "agpl__os_utils__muntrace");

   function Network_Interfaces_Length return Natural;

   type Address_Vector is
     new Agpl.Containers.String_Vectors.Vector with null record;
   subtype Address_Cursor is Agpl.Containers.String_Vectors.Cursor;

   function Ip_Addresses return Address_Vector;
   --  Dot representation x.x.x.x

   type Address_Kinds is (Malformed, Local, Internal, Public);

   function Address_Kind (Address : String) return Address_Kinds;

   procedure At_Exit (Proc : access procedure);
   pragma Import (C, At_Exit, "atexit");
   --  NOTE: these won't be called when Ctrl-C is used,
   --  only on regular or Os_Lib.Exit terminations.

end Agpl.Os_Utils;
