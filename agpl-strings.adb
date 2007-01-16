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
--  $Id: agpl-strings.adb,v 1.3 2004/02/03 22:52:08 Jano Exp $

with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;

--  with Agpl.Text_Io; use Agpl.Text_Io;

package body Agpl.Strings is

   ------------------------------------------------------------------------
   -- Pos                                                                --
   ------------------------------------------------------------------------
   --  Pos of Pattern in S, starting at Start
   --  Returns 0 if not found
   function Pos (S : in String; Pattern : in String; Start : in Positive := 1)
      return Natural
   is
   begin
      return Ada.Strings.Fixed.Index (S (S'First + Start - 1 .. S'Last), Pattern);
   end Pos;

   -----------
   -- Count --
   -----------

   function Count (S, Pattern : String) return Natural is
      Aux   : constant String (1 .. S'Length) := S;
      Found : Natural := Pos (Aux, Pattern);
      Times : Natural := 0;
   begin
      while Found > 0 loop
         Times := Times + 1;
         Found := Pos (Aux, Pattern, Found + 1);
      end loop;
      return Times;
   end Count;

   ------------------------------------------------------------------------
   -- To_lower                                                           --
   ------------------------------------------------------------------------
   function To_lower (This : in String) return String is
   begin
      return Ada.Characters.Handling.To_lower (This);
   end To_lower;

   ------------------------------------------------------------------------
   -- To_upper                                                           --
   ------------------------------------------------------------------------
   function To_upper (This : in String) return String is
   begin
      return Ada.Characters.Handling.To_upper (This);
   end To_upper;

   ------------------------------------------------------------------------
   -- Lpad                                                               --
   ------------------------------------------------------------------------
   function Lpad (
      S    : in String;
      Size : in Natural;
      Fill : in Character := ' ') return String
   is
   begin
      return S & String'(1 .. Size - S'Length => Fill);
   end Lpad;

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   function Rpad (
      S    : in String;
      Size : in Natural;
      Fill : in Character := ' ') return String
   is
   begin
      return String'(1 .. Size - S'Length => Fill) & S;
   end Rpad;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   --  Says if a string is substring of another:
   function Contains (Search_in, Search_for : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Search_in, Search_for) > 0;
   end Contains;

   ------------------------------------------------------------------------
   -- Starts                                                             --
   ------------------------------------------------------------------------
   --  Says if a string starts with some other:
   function Starts (Search_in, Prefix : in String) return Boolean is
   begin
      if Prefix'Length > Search_in'Length then
         return False;
      else
         return Search_in (Search_in'First .. Search_in'First + Prefix'Length - 1) = Prefix;
      end if;
   end Starts;

   -------------
   -- Replace --
   -------------

   function Replace (S, Pattern, New_Pattern : String) return String is
      Result : String (S'First .. S'First + S'Length * New_Pattern'Length);
      Pos    : Positive := Result'First;
      Wold   : constant Natural := Pattern'Length - 1;
      Wnew   : constant Natural := New_Pattern'Length - 1;
      I      : Positive := S'First;
   begin
      while I + Wold <= S'Last loop
         if S (I .. I + Wold) = Pattern then
            Result (Pos .. Pos + Wnew) := New_Pattern;
            I   := I   + Pattern'Length;
            Pos := Pos + New_Pattern'Length;
         else
            Result (Pos) := S (I);
            I   := I   + 1;
            Pos := Pos + 1;
         end if;
      end loop;

      Result (Pos .. Pos + S'Last - I) := S (I .. S'Last);
      Pos := Pos + S'Last - I + 1;

--        Put_Line ("Old:" & S);
--        Put_Line ("Pos:" & Pos'Img);
--        Put_Line ("New:" & Result);

      return Result (Result'First .. Pos - 1);

   end Replace;

   ----------
   -- Left --
   ----------

   function Left (This : in String; Count : Natural) return String is
   begin
      return This (This'First .. Natural'Min (This'Last, This'First + Count - 1));
   end Left;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (This : in String) return String is
   begin
      return U (This (This'First .. This'First)) &
             L (This (This'First + 1 .. This'Last));
   end Capitalize;

end Agpl.Strings;
