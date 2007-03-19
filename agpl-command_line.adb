------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: agpl-command_line.adb,v 1.1 2004/01/21 21:05:52 Jano Exp $

with Ada.Command_line;
with Ada.Text_Io;

with Gnat.Directory_operations;
with Gnat.Os_lib;

package body Agpl.Command_line is

   package Cl  renames Ada.Command_line;
   package Dop renames Gnat.Directory_operations;
   package Ol  renames Gnat.Os_lib;

   ------------------------------------------------------------------------
   --  Exists                                                            --
   ------------------------------------------------------------------------
   --  Ensure an option has been supplied
   function Exists (Option : in String) return Boolean is
   begin
      declare
         Dummy : constant String := Get_Option (Option);
         pragma Unreferenced (Dummy);
      begin
         --  If here, it means the option exists
         return True;
      end;
   exception
      when Option_Not_Supplied =>
         return False;
   end Exists;

   ------------------------------------------------------------------------
   --  Get_Option                                                        --
   ------------------------------------------------------------------------
   --  Returns the string after the given option (f.e: "-f <file>" will return
   --   <file>
   --  May raise Option_Not_Supplied
   function Get_Option (Option : in String) return String is
   begin
      for I in 1 .. Cl.Argument_Count loop
         if Cl.Argument (I) = Option then
            if I < Cl.Argument_Count then
               return Cl.Argument (I + 1);
            else
               return "";
            end if;
         end if;
      end loop;

      raise Option_Not_Supplied with "Missing option: " & Option;
   end Get_Option;

   ------------------------------------------------------------------------
   -- Program_path                                                       --
   ------------------------------------------------------------------------
   --  Returns the path to the executable (with trailing /, without the file).
   function Program_path return String is
   begin
      return Dop.Dir_name (Ol.Normalize_pathname (Cl.Command_name));
   end Program_path;

   ------------------------------------------------------------------------
   -- Program_name                                                       --
   ------------------------------------------------------------------------
   --  Returns the executable's name, without dot extension.
   --  I.e., the same in every platform.
   function Program_name return String is
   begin
      return Dop.Base_name (
         Cl.Command_name,
         Dop.File_extension (Cl.Command_name));
   end Program_name;

   ------------------------
   -- Wait_For_Keystroke --
   ------------------------

   procedure Wait_For_Keystroke is
      C : Character;
      use Ada.Text_IO;
   begin
      Get_Immediate (C);
   end Wait_For_Keystroke;

end Agpl.Command_line;
