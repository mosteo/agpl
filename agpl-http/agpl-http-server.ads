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
--  $Id: agpl-http-server.ads,v 1.6 2004/02/24 15:26:09 Jano Exp $

with Aws.Response;
with Aws.Status;
with Templates_parser;

package Agpl.Http.Server is

   ------------------------------------------------------------------------
   -- Handlers                                                           --
   ------------------------------------------------------------------------
   type Handler_function is access 
      function (Request : in Aws.Status.Data) return Aws.Response.Data;

   type Handler_object is abstract tagged null record;

   -- This function is the one called to obtain the data, and thus the one
   -- you must overload in Handler_object descendant types to implement new
   -- behavior.
   -- This default implementation will simply call Handler.Handler
   function Get_page (
      Handler : in Handler_object;
      Request : in Aws.Status.Data) return Aws.Response.Data is abstract;

   ------------------------------------------------------------------------
   -- Callback_function                                                  --
   ------------------------------------------------------------------------
   -- Default callback function which dispatches to the proper handler
   -- Also acts as a server of static pages.
   -- Also dispatches to the SOAP handler (below) if registered.
   function Callback_function (Request : in AWS.Status.Data)
     return AWS.Response.Data;

   ------------------------------------------------------------------------
   -- Register_user_pass                                                 --
   ------------------------------------------------------------------------
   -- Stores an user/pass; currently only a pair is stored.
   -- The server must have started with session support.
   procedure Register_user_pass (User : in String; Pass : in String);
     
   ------------------------------------------------------------------------
   -- Register_soap                                                      --
   ------------------------------------------------------------------------
   -- Basic SOAP management: only a custom handler can be registered:
   procedure Register_soap (Handler : in Handler_function);

   ------------------------------------------------------------------------
   -- Register handler                                                   --
   ------------------------------------------------------------------------
   -- Register some handler.
   procedure Register_handler (
      URI : in String; Handler : in Handler_object'Class);

   ------------------------------------------------------------------------
   -- Request_Redirect                                                   --
   ------------------------------------------------------------------------
   -- "To" should be the string for a registered handler
   function Request_Redirect (
      From : in Aws.Status.Data; To : in String) return Aws.Response.Data;

   ------------------------------------------------------------------------
   -- Get_root                                                           --
   ------------------------------------------------------------------------
   function Get_root return String;

   ------------------------------------------------------------------------
   -- Set_root                                                           --
   ------------------------------------------------------------------------
   -- Says what's the root folder to prepend to all requests
   procedure Set_root (Root : in String := "");

   ------------------------------------------------------------------------
   -- Get_server_name                                                    --
   ------------------------------------------------------------------------
   function Get_server_name return String;

   ------------------------------------------------------------------------
   -- Set_server_name                                                    --
   ------------------------------------------------------------------------
   -- String used for the @_VERSION_@ tag
   procedure Set_server_name (Version : in String := "");

   ------------------------------------------------------------------------
   -- Set_style_sheet                                                    --
   ------------------------------------------------------------------------
   -- Sets the file to be used as style
   procedure Set_style_sheet (Sheet : in String := "");

   ------------------------------------------------------------------------
   -- Get_style_sheet                                                    --
   ------------------------------------------------------------------------
   function Get_style_sheet return String;

   ------------------------------------------------------------------------
   -- Standard_xlats                                                     --
   ------------------------------------------------------------------------
   -- Returns a translation table for the default Agpl tags:
   -- VERSION <-- Get_server_name
   -- STYLE   <-- Get_style_sheet
   function Standard_xlats (Request : in Aws.Status.Data)
      return Templates_parser.Translate_table;
   pragma Inline (Standard_xlats);

end Agpl.Http.Server;
