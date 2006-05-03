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
--  $Id: agpl-strings-fields.ads,v 1.1 2004/01/25 22:55:11 Jano Exp $

package Agpl.Strings.Fields is

   ------------------------------------------------------------------------
   -- Select_field                                                       --
   ------------------------------------------------------------------------
   --  Returns the Nth field in a string, using the specified separator If no
   --  separator found, returns whole string
   function Select_field
     (S    : in String;
      N    : in Positive;
      C    : in Character := ' ')
      return String;

   ------------------------------------------------------------------------
   -- String_Head                                                        --
   ------------------------------------------------------------------------
   --  All the string if @Separator@ is not found.
   function String_Head
     (S         : String;
      Separator : Character := '/')
      return      String;

   ------------------------------------------------------------------------
   -- String_Tail                                                        --
   ------------------------------------------------------------------------
   --  Returns "" if no @Separator@ found.
   function String_Tail
     (S         : String;
      Separator : Character := '/')
      return      String;

   ------------------------------------------------------------------------
   -- String_Head_Reverse                                                --
   ------------------------------------------------------------------------
   --  These are like above, but from right to left: I.e: Tail(abc/de/fg) =
   --  abc/de ; Head = fg
   function String_Head_Reverse
     (S         : String;
      Separator : Character := '/')
      return      String;

   ------------------------------------------------------------------------
   -- String_Tail_Reverse                                                --
   ------------------------------------------------------------------------
   function String_Tail_Reverse
     (S         : String;
      Separator : Character := '/')
      return      String;

   ------------------------------------------------------------------------
   -- Reverse_String                                                     --
   ------------------------------------------------------------------------
   --  Reverses a string:
   function Reverse_String (S : String) return String;

end Agpl.Strings.Fields;