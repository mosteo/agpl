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

--  Objects for tracing with disc dump optional.
--  This first implementation uses a protected object but not queuing.
--  Writing is done within the protected, which is theoretically illega.
--  Gnat's implementation of I/O allows it so in this first approach we'll
--  leave it like that.

with Gnat.Os_Lib;

with Text_Io; use Text_Io;

package body Agpl.Trace is

   use type Ustring;

   ----------------
   -- Custom_Log --
   ----------------

   procedure Custom_Log
     (This    : in out Object;
      Text    : in     String;
      Level   : in     Levels;
      Section : in     String;
      Logged  : in     Boolean;
      Force   :    out Boolean)
   is
      pragma Unreferenced (This, Text, Level, Section, Logged);
   begin
      Force := False;
   end Custom_Log;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  If File_Name = "" then no output to file is performed.
   --  If Active is false, no action is performed.
   procedure Create (
      This          :    out Object;
      Minimum_Level : in     All_Levels := Informative;
      Console_Echo  : in     Boolean    := False;
      File_Name     : in     String     := "";
      Active        : in     Boolean    := True;
      Clear         : in     Boolean    := False)
   is
      Success : Boolean;
   begin
      if Clear and then File_Name /= "" then
         Gnat.Os_Lib.Delete_File (File_Name, Success);
      end if;

      This.Level  := Minimum_Level;
      This.Echo   := Console_Echo;
      Set_File (This, File_Name);
      This.Active := Active;
   end Create;

   ---------------------
   -- Disable_Section --
   ---------------------

   procedure Disable_Section (This : in out Object; Section : String) is
   begin
      This.Safe.Remove_Section (Section);
   end Disable_Section;

   --------------------
   -- Enable_Section --
   --------------------

   procedure Enable_Section  (This : in out Object; Section : String) is
   begin
      This.Safe.Add_Section (Section);
   end Enable_Section;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin
      This.Safe.Close;
   end Finalize;

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   procedure Log
     (This    : in out Object;
      Text    : in     String;
      Level   : in     Levels;
      Section : in     String := "")
   is
      Doit  : Boolean;
      Force : Boolean;
   begin
      if This.Active then

         Doit :=
           (Level >= This.Level) and then
           (Level >= Error or else Section = "" or else This.Safe.Contains_Section (Section));

         --  Dispatch
         Custom_Log (Object'Class (This),
                     Text,
                     Level,
                     Section,
                     Doit,
                     Force);

         if (Doit and not Force) or else (Force and not Doit) then
            This.Safe.Log (Text, Level);
         end if;
      end if;
   exception
      when E : others =>
         Put_Line ("Trace.Log: " & Report (E));
   end Log;

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  In purpose, This can be null to allow the passing of Null_Object.
   procedure Log
     (This    : in     Object_Access;
      Text    : in     String;
      Level   : in     Levels;
      Section : in String := "") is
   begin
      if This /= null then
         Log (This.all, Text, Level, Section);
      end if;
   end Log;

   ------------------------------------------------------------------------
   -- Log                                                                --
   ------------------------------------------------------------------------
   --  Logs to the default log object.
   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "") is
   begin
      Log (Get_Default_Tracer, Text, Level, Section);
   end Log;

   ------------------------------------------------------------------------
   -- Set_Level                                                          --
   ------------------------------------------------------------------------
   procedure Set_Level (This : in out Object; Level : in All_Levels) is
   begin
      This.Level := Level;
   end Set_Level;

   ------------------------------------------------------------------------
   -- Set_Active                                                         --
   ------------------------------------------------------------------------
   procedure Set_Active (This : in out Object; Active : in Boolean := True) is
   begin
      This.Active := Active;
   end Set_Active;

   ------------------------------------------------------------------------
   -- Set_Echo                                                           --
   ------------------------------------------------------------------------
   procedure Set_Echo (This : in out Object; Echo : in Boolean := True) is
   begin
      This.Echo := Echo;
   end Set_Echo;

   ------------------------------------------------------------------------
   -- Set_File                                                           --
   ------------------------------------------------------------------------

   procedure Set_File (This : in out Object; File : in String) is
   begin
      This.Safe.Set_File (File);
   end Set_File;

   ------------------------------------------------------------------------
   -- Default_Tracer                                                     --
   ------------------------------------------------------------------------
   Default_Tracer : Object_Access := Null_Object;
   pragma Atomic (Default_Tracer);

   ------------------------
   -- Get_Default_Tracer --
   ------------------------

   function Get_Default_Tracer return Object_Access is
   begin
      return Default_Tracer;
   end Get_Default_Tracer;

   ------------------------
   -- Set_Default_Tracer --
   ------------------------

   procedure Set_Default_Tracer (This : in Object_Access := Null_Object) is
   begin
      Default_Tracer := This;
   end Set_Default_Tracer;

   subtype Warn_Prefix is String (1 .. 4);
   type Prefixes is array (All_levels) of Warn_Prefix;

   Prefix : constant Prefixes := (
      Never       => ":n: ",
      Debug       => "-d- ",
      Informative => "(i) ",
      Error       => "[E] ",
      Warning     => "<w> ",
      Always      => "!A! ");

   package SS renames Agpl.Containers.String_Sets;

   ------------------------------------------------------------------------
   -- Protected_Object                                                   --
   ------------------------------------------------------------------------
   protected body Protected_Object is

      --  Add_Section --
      procedure Add_Section (Section : String) is
      begin
         SS.Insert (Sections, Section);
      end Add_Section;

      -- Close --
      procedure Close is
      begin
         if Text_Io.Is_Open (F) then
            Text_Io.Close (F);
         end if;
      end Close;

      --  Contains_Section --
      function Contains_Section (Section : String) return Boolean is
      begin
         return SS.Contains (Sections, Section);
      end Contains_Section;

      -- Log --
      procedure Log (Text : in String; Level : in All_Levels) is
         Msg : constant String := Prefix (Level) & Text;
      begin
         --  To file
         if File /= Null_Ustring then
            begin
               Text_Io.Put_Line (F, Msg);
               if Level >= Error then
                  Text_Io.Flush (F);
               end if;
            exception
               when others =>
                  raise File_IO_Error;
                  --  We are losing the original info, clever :(?
            end;
         end if;
         --  To console
         if Parent.Echo then
            begin
               Text_Io.Put_Line (Msg);
            exception
               when others =>
                  null;
            end;
         end if;
      end Log;

      -- Open --
      procedure Open is
      begin
         if Gnat.Os_Lib.Is_Regular_File (S (File)) then
            Open (F, Mode => Append_File, Name => S (File));
         else
            Create (F, Mode => Append_File, Name => S (File));
         end if;
      end Open;

      --  Remove_Section --
      procedure Remove_Section (Section : String) is
      begin
         SS.Delete (Sections, Section);
      end Remove_Section;

      -- Set_File --
      procedure Set_File (File : in String) is
      begin
         if S (Protected_Object.File) /= File then
            Close;
            Protected_Object.File := U (File);
         end if;
         if File /= "" then
            Open;
         end if;
      exception
         when E : others =>
            Text_Io.Put_Line ("Agpl.Trace.Set_File: " & Report (E));
      end Set_File;

   end Protected_Object;

   Always_Trace : aliased Object;

begin
   Create (Always_Trace,
           Minimum_Level => Debug,
           Console_Echo  => True);
   Set_Default_Tracer (Always_Trace'Access);
end Agpl.Trace;
