with Ada.Streams;
with Ada.Unchecked_Conversion;
with Agpl.Ustrings; use Agpl.Ustrings;

with Agpl.Text_Io; use Agpl.Text_Io;

package body Agpl.Socket_Utils is

   function To_Char is new Ada.Unchecked_Conversion
     (Ada.Streams.Stream_Element, Character);

   function Read_Line (Sock : Gnat.Sockets.Socket_Type) return String is
      Line : Ustring;
      C    : Character;
      E    : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last : Ada.Streams.Stream_Element_Offset;
      use Ada.Streams;
      use Gnat.Sockets;
   begin
      loop
         Receive_Socket (Sock, E, Last);
         if Last < E'First then
            raise Constraint_Error with "socket has been closed";
         else
            C := To_Char (E (E'First));
--              Put_Line ("C: " & C);
            if C = Character'Val (10) then
               return +Line;
            elsif C /= Character'Val (13) then
               Asu.Append (Line, C);
            end if;
         end if;
      end loop;
   end Read_Line;

   procedure Skip_Line (Sock : Gnat.Sockets.Socket_Type) is
      Dummy : constant String := Read_Line (Sock);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Skip_Line;

end Agpl.Socket_Utils;
