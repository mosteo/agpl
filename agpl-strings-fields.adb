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

package body Agpl.Strings.Fields is

   ------------------------------------------------------------------------
   -- Select_field                                                       --
   ------------------------------------------------------------------------
   --  Returns the Nth field in a string, using the specified separator
   function Select_field
     (S    : in String;
      N    : in Positive;
      C    : in Character := ' ')
      return String
   is
      Nth   : Natural := 0;
      Start : Natural := S'First;
   begin
      for I in  S'Range loop
         if S (I) = C then
            Nth := Nth + 1;
            if Nth = N then
               return S (Start .. I - 1);
            else
               Start := I + 1;
            end if;
         end if;
      end loop;

      if N = 1 or else Nth = N - 1 then
         return S (Start .. S'Last);
      else
         return "";
      end if;
   end Select_field;

   ---------------------------
   -- Head / Tail functions --
   ---------------------------

   function String_head
     (s         : String;
      Separator : Character := '/')
      return      String
   is
   begin
      if S = "" then
         return S;
      end if;
      if S (S'First) = Separator then
         return "";
      end if;
      for n in  S'Range loop
         if S (n) = Separator then
            return S (S'First .. n - 1);
         end if;
      end loop;
      return S;
   end String_head;

   --  Returns the head or "" if no tokenizer found.
   function String_tail
     (s         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String
   is
      Skip : Natural := 0;
   begin
      if S = "" then
         return S;
      end if;
      for n in  S'Range loop
         if S (n) = Separator then
            Skip := Skip + 1;
            if Skip = Start and then n < S'Last then
               return S (N + 1 .. S'Last);
            end if;
         end if;
      end loop;
      return "";
   end String_tail;

   --  These are like above, but from right to left: I.e: Tail(abc/de/fg) =
   --  abc/de ; Head = fg
   function String_head_reverse
     (s         : String;
      Separator : Character := '/')
      return      String
   is
   begin
      return Reverse_String (String_Head (Reverse_String (S), Separator));
   end String_head_reverse;

   function String_tail_reverse
     (s         : String;
      Separator : Character := '/')
      return      String
   is
   begin
      return Reverse_String (String_Tail (Reverse_String (S), Separator));
   end String_tail_reverse;

   --  Reverses a string:
   function Reverse_string (s : String) return String is
      r : String (S'Range);
   begin
      for n in  S'Range loop
         r (r'Last + (S'First - n))   := S (n);
      end loop;
      return r;
   end Reverse_string;

end Agpl.Strings.Fields;
