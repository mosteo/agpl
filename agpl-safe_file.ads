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
--  $Id: agpl-safe_file.ads,v 1.3 2004/01/21 21:05:25 Jano Exp $

--  Facilities for saving/loading from a file in a way that it doesn't
--  overwrite the previous version.
--  When opening for writing, a temporary is used. After completion, the
--  original file is removed and the temporary renamed.
--  When opening for reading, regular filename is tried. If missing, a check
--  for a temporary is made, and if it exists, it is renamed to the regular
--  name and opened. If neither regular and temporary exists, error.

with Ada.Streams.Stream_IO;
use  Ada.Streams;
use  Ada;

package Agpl.Safe_file is

   --  pragma Elaborate_Body;

   File_Not_Found : exception;

   ------------------------------------------------------------------------
   -- Exists_for_reading                                                 --
   ------------------------------------------------------------------------
   function Exists_for_reading (Name : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Get_Real_Name                                                      --
   ------------------------------------------------------------------------
   --  Gets the real name found (i.e. the supplied or the backup one
   --  May raise File_Not_Found
   function Get_Real_Name (Name : in String) return String;

   ------------------------------------------------------------------------
   -- Open                                                               --
   ------------------------------------------------------------------------
   procedure Open (
      File : in out Stream_IO.File_type;
      Mode : in     Stream_IO.File_mode;
      Name : in     String := "";
      Form : in     String := "");

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   procedure Close (File : in out Stream_IO.File_type);

end Agpl.Safe_file;
