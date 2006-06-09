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

--  Version for handlers which return single values (no vectors)

with Agpl.Http.Server.Sort_handler;

package Agpl.Http.Server.Single_handler is

   --  Void vector procedure:
   procedure Null_vector (Data : out Sort_handler.Data_set);
   --  Void singleton
   function Null_singleton return Templates_Parser.Translate_Table renames
      Sort_handler.Void_singleton;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- I.e., must be specified using the Single and Page constraints:
   type Object
      (
      Single : Sort_handler.Singleton_function;
      Page   : Sort_handler.Source_page)
   is new Sort_handler.Object
      (Source => Null_vector'Access,
      Single => Single,
      Page => Page) with null record;

end Agpl.Http.Server.Single_handler;
