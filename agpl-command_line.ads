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
--  $Id: agpl-command_line.ads,v 1.1 2004/01/21 21:05:52 Jano Exp $

package Agpl.Command_line is

   pragma Elaborate_Body;

   Option_Not_Supplied : exception;

   ------------------------------------------------------------------------
   --  Exists                                                            --
   ------------------------------------------------------------------------
   --  Ensure an option has been supplied
   function Exists (Option : in String) return Boolean;

   ------------------------------------------------------------------------
   --  Get_Option                                                        --
   ------------------------------------------------------------------------
   --  Returns the string after the given option (f.e: "-f <file>" will return
   --   <file>
   --  May raise Option_Not_Supplied
   function Get_Option (Option : in String) return String;

   ------------------------------------------------------------------------
   -- Program_path                                                       --
   ------------------------------------------------------------------------
   --   Returns the path to the executable (with trailing /, without the file).
   function Program_path return String;

   ------------------------------------------------------------------------
   -- Program_name                                                       --
   ------------------------------------------------------------------------
   --  Returns the executable's name, without dot extension.
   --  I.e., the same in every platform.
   function Program_name return String;

   ------------------------
   -- Wait_For_Keystroke --
   ------------------------
   procedure Wait_For_Keystroke;
   --  Waits until some key is pressed in the command line.

end Agpl.Command_line;
