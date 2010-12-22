with Gnat.Sockets;

package Agpl.Socket_Utils is

   function Read_Line (Sock : Gnat.Sockets.Socket_Type) return String;
   --  Read a line from a socket until \0x0a is read
   --  No 0x13 are expected to be seen...

   procedure Skip_Line (Sock : Gnat.Sockets.Socket_Type);

end Agpl.Socket_Utils;
