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
with Agpl.Trace_Is;

with Ada.Exceptions;
with Ada.Tags;         use Ada.Tags;
use Ada;

package Agpl.Trace is

   pragma Preelaborate;

   Enabled : Boolean renames Trace_Is.Enabled;
   --  I expect that inlining will cause no calls when this is false.

   --  Root for logging facilities
   --  This one does output to Stdout only.

   --  Error level messages are shown even if its section is not enabled!!

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is synchronized interface;
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

   Null_Object : constant Object_Access := null;

   ---------------------
   -- Disable_Section --
   ---------------------
   procedure Disable_Section (This : in out Object; Section : String)
   is abstract;
   procedure Enable_Section  (This : in out Object; Section : String)
   is abstract;

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "") is abstract;

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  In purpose, This can be null to allow the passing of Null_Object.
   procedure Log
     (This    : in Object_Access;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "");
   pragma Inline (Log);

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  Logs to the default log object.
   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "");
   pragma Inline (Log);

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
   procedure Set_Level (This : in out Object; Level : in All_Levels)
   is abstract;

   ------------------------------------------------------------------------
   -- Set_Active                                                         --
   ------------------------------------------------------------------------
   procedure Set_Active (This : in out Object; Active : in Boolean := True)
   is abstract;

   ------------------------------------------------------------------------
   -- Default_Tracer                                                     --
   ------------------------------------------------------------------------
   function Get_Default_Tracer return Object_Access;
   pragma Inline (Get_Default_Tracer);

   procedure Set_Default_Tracer (This : in Object_Access := Null_Object);
   pragma Inline (Set_Default_Tracer);

end Agpl.Trace;
