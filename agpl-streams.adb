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
--  $Id: agpl-streams.adb,v 1.1 2004/02/29 20:36:41 Jano Exp $

with Ada.Unchecked_Conversion;

package body Agpl.Streams is

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (This : in String)
                                     return    Ada.Streams.Stream_Element_Array
   is
      subtype StringX is String (1 .. This'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. This'Length);
      function To_Arr is new Ada.Unchecked_Conversion (StringX, ArrayX);
   begin
      return To_Arr (This);
   end To_Stream_Element_Array;

   ---------------
   -- To_string --
   ---------------

   function To_string (This : in Ada.Streams.Stream_element_array)
      return String
   is
      subtype StringX is String (1 .. This'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. This'Length);
      function To_Str is new Ada.Unchecked_Conversion (ArrayX, StringX);
   begin
      return To_Str (This);
   end To_string;

end Agpl.Streams;
