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
--  $Id: agpl-http-server-sort_handler.ads,v 1.4 2004/02/03 22:52:08 Jano Exp $

with Aws.Response;
with Aws.Status;
with Templates_parser;

with Agpl.Dynamic_vector;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Agpl.Http.Server.Sort_handler is

   Default_Lines_Per_Page : constant := 1000;

   ---------------
   -- Data_cell --
   ---------------
   -- Each cell will be passed in this record:
   type Data_cell is record
      Value       : Ustring; -- Data to be displayed
      Order_value : Ustring; -- Value to sort with
   end record;

   -- Each row is an array of Data_entries:
   package Datarows is new
      Agpl.Dynamic_vector (Data_cell, Initial_size => 20);

   --------------
   -- Data_row --
   --------------
   type Data_row is new Datarows.Object (First => 1) with null record;

   -- The entire dataset is a dynamic vector of Data_rows:
   package Datasets is new 
      Agpl.Dynamic_vector (
         Data_row, Initial_size => 100, Grow_factor => 2.0);

   --------------
   -- Data_set --
   --------------
   type Data_set is new Datasets.Object (First => 1) with null record;

   -- The Sort_Handler will work with functions that return datasets:
   type Source_procedure is access procedure (Data : out Data_set);

   -- And also with functions that return singletons (values not in tables):
   type Singleton_function is access 
      function return Templates_parser.Translate_table;

   -- The source page is simply an access to a string:
   type Source_page is access String;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object (
      Source : Source_procedure;
      Single : Singleton_function;
      Page   : Source_page)
   is new Handler_object with private;

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   -- There are several arguments that can be specified in the HTTP request:
   -- orden=positive    : column to order the listing
   -- sentido=boolean   : true=descending, false=ascending
   -- page=positive     : which page of the listing to retrieve
   -- lines=positive    : num of lines in each page
   -- A special tag PAGER will be replaced with links for the pages.
   -- It has class="pager".
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data;
   
   ------------------------------------------------------------------------
   -- Set_Lines_Per_Page                                                 --
   ------------------------------------------------------------------------
   -- To change the default number of records per page.
   procedure Set_Lines_Per_Page (This : in out Object; Lines : in Positive);

   ------------------------------------------------------------------------
   -- Set_settings_file                                                  --
   ------------------------------------------------------------------------
   -- To indicate where (path + name) to save ordering prefs.
   procedure Set_settings_file (This : in String);

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   -- Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton return Templates_parser.Translate_table;
   pragma Inline (Void_singleton);
   function Null_singleton return Templates_parser.Translate_table
      renames Void_singleton;

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   -- Auxiliary to ease creation of sorting fields based in integers
   function Rpad (I : in Integer;  Size : in Natural := 11) return Ustring;
   function Rpad (I : in Float;    Size : in Natural := 11) return Ustring;
   function Rpad (I : in Duration; Size : in Natural := 11) return Ustring;

private

   type Object (
      Source : Source_procedure;
      Single : Singleton_function;
      Page   : Source_page)
   is new Handler_object with record
      Lines_Per_Page : Positive := Default_Lines_Per_Page;
   end record;

end Agpl.Http.Server.Sort_handler;
