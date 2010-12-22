with Agpl.Interfaces.C.Types;
with Agpl.Strings;
with Agpl.Strings.Fields;
--  with Agpl.Trace; use Agpl.Trace;
with Interfaces.C;
with Interfaces.C.Strings;

package body Agpl.Os_Utils is

   package Ic renames Standard.Interfaces.C;
   package Agpl_C renames Agpl.Interfaces.C.Types;

   function Get_Free_Disk_Space (Path : String := ".") return Float
   is
      C_Path : constant Agpl_C.Cstring := Agpl_C.New_Cstring (Path);

      function Internal (Path : Ic.Strings.Chars_Ptr) return Ic.double;
      pragma Import (C, Internal, "agpl__os_utils__get_free_disk_space_c");
   begin
      return Float (Internal (C_Path.Ptr));
   end Get_Free_Disk_Space;

   -----------
   -- Spawn --
   -----------

   function Spawn (Command : String) return Integer is
      Cc : constant Agpl_C.Cstring := Agpl_C.New_Cstring (Command);

      function Spawn_C (Command : Ic.Strings.Chars_Ptr) return Ic.Int;
      pragma Import (C, Spawn_C, "agpl__os_utils__spawn_c");
   begin
      return Integer (Spawn_C (Cc.Ptr));
   end Spawn;

   -------------------------------
   -- Network_Interfaces_Length --
   -------------------------------

   function Network_Interfaces_Length return Natural is
      function C_Internal return Agpl_C.Int;
      pragma Import (C, C_Internal, "agpl__os_utils__num_ifaces");
   begin
      return Natural (C_Internal);
   end Network_Interfaces_Length;

   ------------------
   -- Ip_Addresses --
   ------------------

   function Ip_Addresses return Address_Vector is
      Addresses : Address_Vector;

      function C_Internal (Index : Agpl_C.Int;
                           Addr  : Ic.Strings.Chars_Ptr) return Agpl_C.Return_Code;
      pragma Import (C, C_Internal, "agpl__os_utils__iface_addr");
   begin
      for I in 1 .. Network_Interfaces_Length loop
         declare
            use Agpl_C;
            Addr : constant Agpl_C.Cstring :=
              Agpl_C.New_Cstring ("255.255.255.255");
         begin
            if C_Internal (Agpl_C.Int (I), Addr.Ptr) = Return_Ok then
               Addresses.Append (Addr.Value);
            else
               raise Program_Error with "IP_Addresses internal call failed";
            end if;
         end;
      end loop;

      return Addresses;
   end Ip_Addresses;

   ------------------
   -- Address_Kind --
   ------------------

   function Address_Kind (Address : String) return Address_Kinds is
      use Agpl.Strings;
      use Agpl.Strings.Fields;

      function Is_Valid return Boolean is
      begin
         if Num_Tokens (Address, '.') /= 4 then
            return False;
         end if;

         for I in 1 .. 4 loop
            declare
               Val : constant Integer :=
                 Integer'Value (Select_Field (Address, I, '.'));
            begin
               if Val < 0 or else Val > 255 or else
                 Trim (Val'Img) /= Select_Field (Address, I, '.')
               then
                  return False;
               end if;
            end;
         end loop;

         return True;
      exception
         when others =>
            return False;
      end Is_Valid;

   begin
      if not Is_Valid then
         return Malformed;
      end if;

      if Head (Address, '.') = "0" then
         return Malformed;
      elsif Head (Address, '.') = "127" then
         return Local;
      elsif Head (Address, '.') = "10" then
         return Internal;
      elsif Head (Address, '.') = "192" and then
        Select_Field (Address, 2, '.') = "168"
      then
         return Internal;
      elsif Head (Address, '.') = "172" and then
        Natural'Value (Select_Field (Address, 2, '.')) in 16 .. 31
      then
         return Internal;
      else
         return Public;
      end if;
   end Address_Kind;

end Agpl.Os_Utils;
