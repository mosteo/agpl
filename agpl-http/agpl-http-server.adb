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
--  $Id: agpl-http-server.adb,v 1.6 2004/02/24 15:26:09 Jano Exp $

with Agpl.Debug;
with Agpl.Exceptions;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Aws.Messages;
with Aws.Mime;
with Aws.Parameters;
with Aws.Resources.Embedded;
with AWS.Response;
with AWS.Status;
with AWS.Status.Set;
use  AWS;

with Templates_parser;

with Charles.Maps.Hashed.Strings.Unbounded;
with Charles.Hash_string;

with Gnat.Os_lib;
use  Gnat;

with Text_Io;

package body Agpl.Http.Server is

   -- Root:
   Root        : Ustring;
   -- Server name:
   Server_name : Ustring;
   -- Style sheet:
   Style_sheet : Ustring;

   -- Auth:
   Username : Ustring;
   Password : Ustring;
   
   -- Handlers:
   type Handler_access is access Handler_object'Class;
   package Disp_tables is new Charles.Maps.Hashed.Strings.Unbounded (
      Handler_access, Charles.Hash_string, "=", "="); 
   Dispatchers : Disp_tables.Container_type;

   -- Ahead declaration
   function Dispatcher_404 (Request : in Aws.Status.Data) 
      return Aws.Response.Data;

   -- SOAP handler
   Soap_handler : Handler_function;

   ---------------
   -- Authorize --
   ---------------
   function Authorize (Request : in AWS.Status.Data) 
      return Aws.Response.Data
   is
   begin
      return Aws.Response.Authenticate (
         "You are coming from " & Aws.Status.Peername (Request) &
         ". Your papers, please:", Aws.Response.Basic);
   end Authorize;

   ----------------
   -- Check_user --
   ----------------
   function Check_user (Request : in AWS.Status.Data) return Boolean is
      User : constant String := Aws.Status.Authorization_Name (Request);
      Pwd  : constant String := Aws.Status.Authorization_Password (Request);
      Addr  :constant String := Aws.Status.Peername (Request);
   begin
      if Addr'Length < 9 or else 
         Addr (Addr'First .. Addr'First + 8) /= "127.0.0.1" 
      then
         if User /= S (Username) or else Pwd /= S (Password) then
            -- Small delay to difficult dictionary attacks:
            delay 1.0;
            return false;
         end if;
      end if;

      return true;
   end Check_user;

   -----------------------
   -- Callback function --
   -----------------------
   function Callback_function (Request : in AWS.Status.Data)
     return AWS.Response.Data is
     use Disp_tables;
     use Templates_parser;
     URI  : constant String := Aws.Status.URI (Request);
     I    : constant Iterator_type := Find (Dispatchers, URI);
     Path : constant String := S (Root) & URI (URI'first + 1 .. URI'last);
   begin
      if not Check_user (Request) then
         return Authorize (Request);
      end if;

      if AWS.Status.Is_SOAP(Request) then
         if Soap_handler /= null then
            return Soap_handler (Request);
         else
            raise Agpl.Exceptions.Unimplemented;
         end if;
      else
         -- Dispatch according to the request:
         if I /= Back (Dispatchers) then
            return Get_Page (Element (I).all, Request);
         elsif Os_lib.Is_regular_file (Path) or else
            Aws.Resources.Embedded.Exists (Path)
         then
            if Aws.Mime.Is_text (Aws.Mime.Content_type (Path)) then
               return Aws.Response.Build (
                  Aws.Mime.Content_type (Path), 
                  Ustring'(Parse (Path, Standard_xlats (Request))),
                  Cache_control => Aws.Messages.No_cache);
            else
               return Aws.Response.File (Aws.Mime.Content_type (Path), Path);
            end if;
         else
            return Dispatcher_404 (Request);
         end if;
      end if;
   exception
      when E : others =>
         Text_Io.Put_Line ("Callback_Function: " & Agpl.Debug.Report (E));
         raise;
   end Callback_function;

   ------------------------------------------------------------------------
   -- Register_user_pass                                                 --
   ------------------------------------------------------------------------
   -- Stores an user/pass; currently only a pair is stored.
   -- The server must have started with session support.
   procedure Register_user_pass (User : in String; Pass : in String) is
   begin
      Username := U (User);
      Password := U (Pass);
   end Register_user_pass;

   ------------------------------------------------------------------------
   -- Register_soap                                                      --
   ------------------------------------------------------------------------
   -- Basic SOAP management: only a custom handler can be registered:
   procedure Register_soap (Handler : in Handler_function) is
   begin
      Soap_handler := Handler;
   end Register_soap;

   ------------------------------------------------------------------------
   -- Register_handler                                                   --
   ------------------------------------------------------------------------
   procedure Register_handler (
      URI : in String; Handler : in Handler_object'Class) is
      use Disp_tables;
   begin
      Insert (Dispatchers, URI, new Handler_object'Class'(Handler));
   end Register_handler;

   ------------------------------------------------------------------------
   -- Request_Redirect                                                   --
   ------------------------------------------------------------------------
   function Request_Redirect (
      From : in Aws.Status.Data; To : in String) return Aws.Response.Data 
   is
   begin
      declare
         New_req : Aws.Status.Data := From; -- To copy auth data
      begin
         Aws.Status.Set.Request (
            New_req, Aws.Status.GET, To, Aws.Http_version);
         return Agpl.Http.Server.Callback_function (New_req);
      end;
   end Request_Redirect;

   ------------------------------------------------------------------------
   -- Dispatcher_404                                                     --
   ------------------------------------------------------------------------
   function Dispatcher_404 (Request : in Aws.Status.Data)
      return Aws.Response.Data 
   is
      use Disp_tables;
      use Templates_parser;
      I : Iterator_type := First (Dispatchers);
      V : Vector_tag;
   begin
      while I /= Back (Dispatchers) loop
         V := V & Key (I);
         I := Succ (I);
      end loop;
      declare
         Translat : Translate_table := (
            1 => Assoc ("SERVICE", V),
            2 => Assoc ("VERSION", Server_name));
      begin
         return Aws.Response.Acknowledge (
            Aws.Messages.S404,
            String'(
               Parse (S (Root) & "err404.html", 
                  Translat & Standard_xlats (Request))),
            Aws.Mime.Text_html);
      end;
   end Dispatcher_404;

   ------------------------------------------------------------------------
   -- Get_root                                                           --
   ------------------------------------------------------------------------
   function Get_root return String is
   begin
      return S (Root);
   end Get_root;

   ------------------------------------------------------------------------
   -- Set_root                                                           --
   ------------------------------------------------------------------------
   -- Says what's the root folder to prepend to all requests
   procedure Set_root (Root : in String := "") is
   begin
      Server.Root := U (Root);
   end Set_root;

   ------------------------------------------------------------------------
   -- Get_server_name                                                    --
   ------------------------------------------------------------------------
   function Get_server_name return String is
   begin
      return S (Server_name);
   end Get_server_name;

   ------------------------------------------------------------------------
   -- Set_server_name                                                    --
   ------------------------------------------------------------------------
   -- String used for the @_VERSION_@ tag
   procedure Set_server_name (Version : in String := "") is
   begin
      Server_name := U (Version);
   end Set_server_name;

   ------------------------------------------------------------------------
   -- Set_style_sheet                                                    --
   ------------------------------------------------------------------------
   -- Sets the file to be used as style
   procedure Set_style_sheet (Sheet : in String := "") is
   begin
      Style_sheet := U (Sheet);
   end Set_style_sheet;

   ------------------------------------------------------------------------
   -- Get_style_sheet                                                    --
   ------------------------------------------------------------------------
   function Get_style_sheet return String is
   begin
      return S (Style_sheet);
   end Get_style_sheet;

   ------------------------------------------------------------------------
   -- Standard_xlats                                                     --
   ------------------------------------------------------------------------
   -- Replaces standard tags for all pages:
   -- VERSION <-- Get_server_name
   -- STYLE   <-- Get_style_sheet
   -- URI     <-- Uri of the request
   function Standard_xlats (Request : in Aws.Status.Data)
      return Templates_parser.Translate_table is
      use Templates_parser;
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
   begin
      return (
         Assoc ("STYLE",   Get_style_sheet),
         Assoc ("VERSION", Get_server_name),
         Assoc ("URI",     Aws.Status.URI (Request) & 
                           Aws.Parameters.Uri_format (Params))
         );
   end;

end Agpl.Http.Server;
