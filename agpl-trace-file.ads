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
--  $Id: agpl.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

--  Just implements sections but still doesn't trace.

with Agpl.Calendar.Format;
with Agpl.Command_Line;
with Agpl.Trace.Root;

private with Ada.Finalization;
with Ada.Text_Io;

package Agpl.Trace.File is

   pragma Elaborate_Body;

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   type Modes is (Append, Reset);

   overriding
   procedure Log (This    : in out Object;
                  Text    : in String;
                  Level   : in Levels;
                  Section : in String := "");

   not overriding
   procedure Set_File
     (This : in out Object;
      Name : in     String := Command_Line.Program_Name & "." &
                              Calendar.Format.Datestamp (Separator =>'.') &"."&
                              Calendar.Format.Timestamp & ".log";
      Mode : in     Modes  := Append);

private

   type Destructor_Type (Parent : access Object) is new
     Ada.Finalization.Limited_Controlled with null record;

   type Object is new Root.Object with record
      File       : Ada.Text_Io.File_Type;
      Opened     : Boolean := False;

      Destructor : Destructor_Type (Object'Access);
   end record;

   procedure Finalize (This : in out Destructor_Type);

end Agpl.Trace.File;
