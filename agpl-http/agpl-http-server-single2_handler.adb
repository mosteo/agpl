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
--  $Id: agpl-http-server-sort_handler.adb,v 1.9 2004/02/04 21:31:02 Jano Exp $

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Aws.Messages;
with Aws.Mime;

package body Agpl.Http.Server.Single2_handler is

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data 
   is
      use Templates_parser;
   begin
      return 
         Aws.Response.Build (
            Aws.Mime.Content_type (This.Page.all),
            UString'(Parse (
               Get_root & This.Page.all, 
               Standard_xlats (Request) & This.Single (Request), 
               Cached => false)),
            Cache_control => Aws.Messages.No_cache);
   end Get_page;

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   -- Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton (Request : in Aws.Status.Data) return Templates_parser.Translate_table is
      pragma Unreferenced (Request);
   begin
      return Templates_parser.No_translation;
   end Void_singleton;

end Agpl.Http.Server.Single2_handler;