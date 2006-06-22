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

--  Objects for tracing with disc dump optional. This first implementation uses
--  a protected object but not queuing. Writing is done within the protected,
--  which is theoretically illega. Gnat's implementation of I/O allows it so in
--  this first approach we'll leave it like that.

with Agpl.Debug;
with Agpl.Containers.String_Sets;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Tags;         use Ada.Tags;
use Ada;

package Agpl.Trace.Console is

   pragma Elaborate_Body;

   --  Error level messages are shown even if its section is not enabled!!

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   --  Happens when trying to log to file:
   File_Io_Error : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is tagged limited private;
   type Object_Access is access all Object'Class;

   type Levels is (
      Never,
      Debug,
      Informative,
      Warning,
      Error,
      Always);
   subtype All_Levels     is Levels     range Never .. Always;
   subtype Warning_levels is All_Levels range Debug .. Error;

   No_File     : constant String;
   Null_Object : constant Object_Access;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  If File_Name = "" then no output to file is performed. If Active is
   --  false, no action is performed. If clear is true, the file will be
   --  erased.
   --  Minimum_Level says the minimum relevance a message has to have to be logged.
   procedure Create
     (This          : out Object;
      Minimum_Level : in All_Levels := Informative;
      Console_Echo  : in Boolean    := False;
      File_Name     : in String     := "";
      Active        : in Boolean    := True;
      Clear         : in Boolean    := False);

   ----------------
   -- Custom_Log --
   ----------------
   procedure Custom_Log
     (This    : in out Object;
      Text    : in     String;
      Level   : in     Levels;
      Section : in     String;
      Logged  : in     Boolean;
      Force   :    out Boolean);
   --  Called for descendent types. Logged says if the text complies with
   --  the criteria of level and section to be logged.
   --  Force will cause the opposite behavior at return.
   --  This call is not thread safe.

   ---------------------
   -- Disable_Section --
   ---------------------
   procedure Disable_Section (This : in out Object; Section : String);
   procedure Enable_Section  (This : in out Object; Section : String);

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "");

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  In purpose, This can be null to allow the passing of Null_Object.
   procedure Log
     (This    : in Object_Access;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "");

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  Logs to the default log object.
   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "");

   ------------------
   -- External_Tag --
   ------------------
   function External_Tag (Tag : in Ada.Tags.Tag) return String renames
     Ada.Tags.External_Tag;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   --  Constructs a error string upon exception:
   function Report (E : Ada.Exceptions.Exception_Occurrence) return String
      renames Agpl.Debug.Report;

   ------------------------------------------------------------------------
   -- Set_Level                                                          --
   ------------------------------------------------------------------------
   --  The minimum!
   procedure Set_Level (This : in out Object; Level : in All_Levels);

   ------------------------------------------------------------------------
   -- Set_Active                                                         --
   ------------------------------------------------------------------------
   procedure Set_Active (This : in out Object; Active : in Boolean := True);

   ------------------------------------------------------------------------
   -- Set_Echo                                                           --
   ------------------------------------------------------------------------
   procedure Set_Echo (This : in out Object; Echo : in Boolean := True);

   ------------------------------------------------------------------------
   -- Set_File                                                           --
   ------------------------------------------------------------------------
   procedure Set_File (This : in out Object; File : in String);

   ------------------------------------------------------------------------
   -- Default_Tracer                                                     --
   ------------------------------------------------------------------------
   function Get_Default_Tracer return Object_Access;
   pragma Inline (Get_Default_Tracer);

   procedure Set_Default_Tracer (This : in Object_Access := Null_Object);
   pragma Inline (Set_Default_Tracer);

   ---------------
   -- Auxiliary --
   ---------------

   Stdout : Text_IO.File_Type renames Text_IO.Standard_Output;
   Stderr : Text_IO.File_Type renames Text_IO.Standard_Error;

private

   No_File     : constant String        := "";
   Null_Object : constant Object_Access := null;

   protected type Protected_Object (Parent : access Object) is
      procedure Add_Section (Section : String);
      procedure Close;
      function  Contains_Section (Section : String) return Boolean;
      procedure Log (Text : in String; Level : in All_Levels);
      procedure Open;
      procedure Remove_Section (Section : String);
      procedure Set_File (File : in String);
   private
      File     : UString := Null_Ustring;
      F        : Text_IO.File_Type;
      Sections : Containers.String_Sets.Set;
   end Protected_Object;

   type Object is new Finalization.Limited_Controlled with record
      Active : Boolean := True;
      pragma Atomic (Active);
      Echo : Boolean := False;
      pragma Atomic (Echo);
      Level : All_Levels := Informative;
      pragma Atomic (Level);

      Safe : Protected_Object (Object'Access);
   end record;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object);

end Agpl.Trace.Console;
