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
--  $Id: agpl-types.ads,v 1.2 2004/02/29 20:36:41 Jano Exp $

with Agpl.Dynamic_Vector;

with Ada.Streams;
with Ada.Strings.Unbounded;

package Agpl.Types.Ustrings is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Unbounded String Facilities                                        --
   ------------------------------------------------------------------------
   package ASU renames Ada.Strings.Unbounded;

   subtype UString is ASU.Unbounded_String;

   type Ustring_Array is array (Positive range <>) of Ustring;

   function To_String (U : UString) return String  renames ASU.To_string;

   function To_Ustring (S : String) return UString renames ASU.To_unbounded_string;

   function S (U : UString)         return String  renames ASU.To_string;

   function U (S : String)          return UString renames ASU.To_unbounded_string;

   function "+" (U : UString)       return String  renames ASU.To_string;

   function "+" (S : String)        return UString renames ASU.To_unbounded_string;

   Null_Ustring : UString renames ASU.Null_Unbounded_String;

   ------------------------------------------------------------------------
   -- Write_To_Stream                                                    --
   ------------------------------------------------------------------------
   --  Writes the Ustring contents to a stream
   procedure Write_To_Stream (
      This : in Ustring; Stream : access Ada.Streams.Root_Stream_Type'Class);

   ------------------------------------------------------------------------
   -- Package for dynamic arrays                                         --
   ------------------------------------------------------------------------
   package Ustring_Vector is new Agpl.Dynamic_Vector (Ustring);

end Agpl.Types.Ustrings;
