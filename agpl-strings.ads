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
--  $Id: agpl-strings.ads,v 1.3 2004/02/03 22:52:09 Jano Exp $

with Agpl.Conversions;

package Agpl.Strings is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   --  Says if a string is substring of another:
   function Contains (Search_in, Search_for : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Pos                                                                --
   ------------------------------------------------------------------------
   --  Pos of Pattern in S, starting at S'First + Start - 1
   --  Returns 0 if not found
   function Pos (S : in String; Pattern : in String; Start : in Positive := 1)
      return Natural;

   ------------------------------------------------------------------------
   -- Lpad                                                               --
   ------------------------------------------------------------------------
   function Lpad (S    : in String;
                  Size : in Natural;
                  Fill : in Character := ' ') return String;
   pragma Inline (Lpad);

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   function Rpad (S    : in String;
                  Size : in Natural;
                  Fill : in Character := ' ') return String;
   pragma Inline (Rpad);

   ------------------------------------------------------------------------
   -- Starts                                                             --
   ------------------------------------------------------------------------
   --  Says if a string starts with some other:
   function Starts (Search_in, Prefix : in String) return Boolean;

   ------------------------------------------------------------------------
   -- To_lower                                                           --
   ------------------------------------------------------------------------
   function To_lower (This : in String) return String;
   function L        (This : in String) return String renames To_lower;
   pragma Inline (To_lower);

   ------------------------------------------------------------------------
   -- To_upper                                                           --
   ------------------------------------------------------------------------
   function To_upper (This : in String) return String;
   function U        (This : in String) return String renames To_upper;
   pragma Inline (To_upper);

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   function To_String (N : Integer) return String
                       renames Conversions.To_String;

   function To_String (N : Float; Decimals : Natural := 2) return String
                       renames Conversions.To_String;
   function S (N : Float; Decimals : Natural := 2) return String
                       renames To_String;

   ------------------------------------------------------------------------
   -- Trim                                                               --
   ------------------------------------------------------------------------
   function Trim (This : in String) return String;
   pragma Inline (Trim);


end Agpl.Strings;
