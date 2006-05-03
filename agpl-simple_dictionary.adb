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
--  $Id: agpl.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

--  Provides a one-way hash table with a single value per key.

package body Agpl.Simple_Dictionary is

   use Element_Map;

   ------------------------------------------------------------------------
   -- Add_Word                                                           --
   ------------------------------------------------------------------------
   -- Add a word with given index (key).
   procedure Add_Word (This : in out Object; Key : in String; Element : in Element_Type) is
   begin
      Replace_Element (This, Key, Element);
      
      -- Correctly replaced?
      pragma Assert (Element_Map.Element (Find (This, Key)) = Element);
   end Add_Word;

   ------------------------------------------------------------------------
   -- Are_Compatible                                                     --
   ------------------------------------------------------------------------
   -- True if elements in both containers are equal, extra are ignored.
   -- Commutative.
   function Are_Compatible (L, R : in Object) return Boolean is
      I, J    : Iterator_Type;
      Matched : Boolean := false;
   begin
      I := First (L);
      while I /= Back (L) loop
         J := Find (R, Key (I));
         if J /= Back (R) then
            if not Equal (Element (I), Element (J)) then
               return false;
            else
               Matched := true;
            end if;
         end if;
         I := Succ (I);
      end loop;

      return Matched;
   end Are_Compatible;

   ------------------------------------------------------------------------
   -- Contains_Key                                                       --
   ------------------------------------------------------------------------
   -- True if the dictionary contains the given key
   function Contains_Key (This : in Object; Key : in String) return Boolean is
   begin
      return Is_In (Key, This);
   end Contains_Key;

   ------------------------------------------------------------------------
   -- Get_Contents                                                       --
   ------------------------------------------------------------------------
   -- Return an array of contents in the dictionary
   function Get_Contents (This : in Object) return Pair_Array is
      I   : Iterator_Type := First (This);
      Res : Pair_Array (1 .. Length (This));
   begin
      for J in Res'Range loop
         Res (J).Key   := U (Key (I));
         Res (J).Value := Element (I);
         I := Succ (I);
      end loop;
      return Res;
   end Get_Contents;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   function Get_Value (This : in Object; Key : in String) return Element_Type is
   begin
      return Element (Find (This, Key));
   end Get_Value;

   ------------------------------------------------------------------------
   -- Merge                                                              --
   ------------------------------------------------------------------------
   -- Adds elements not in Former from Later.
   -- No compatibility check is performed
   procedure Merge (Former : in out Object; Later : in Object) is
      I : Iterator_Type := First (Later);
   begin
      while I /= Back (Later) loop
         Add_Word (Former, Key (I), Element (I));
         I := Succ (I);
      end loop;
   end Merge;
      
end Agpl.Simple_Dictionary;
