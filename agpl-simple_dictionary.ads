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

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Charles.Hash_String;
with Charles.Maps.Hashed.Strings.Unbounded;

generic
   type Element_Type is private;
   with function Equal (L, R : in Element_Type) return boolean;
package Agpl.Simple_Dictionary is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is private;

   type Pairs is record
      Key   : Ustring;
      Value : Element_Type;
   end record;
   type Pair_Array is array (Positive range <>) of Pairs;

   ------------------------------------------------------------------------
   -- Add_Word                                                           --
   ------------------------------------------------------------------------
   -- Add a word with given index (key).
   procedure Add_Word (This : in out Object; Key : in String; Element : in Element_Type);

   ------------------------------------------------------------------------
   -- Are_Compatible                                                     --
   ------------------------------------------------------------------------
   -- True if elements in both containers are equal, extra are ignored.
   -- At least one element must match.
   -- Commutative.
   function Are_Compatible (L, R : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Contains_Key                                                       --
   ------------------------------------------------------------------------
   -- True if the dictionary contains the given key
   function Contains_Key (This : in Object; Key : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Get_Contents                                                       --
   ------------------------------------------------------------------------
   -- Return an array of contents in the dictionary
   function Get_Contents (This : in Object) return Pair_Array;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   function Get_Value (This : in Object; Key : in String) return Element_Type;

   ------------------------------------------------------------------------
   -- Merge                                                              --
   ------------------------------------------------------------------------
   -- Adds elements not in Former from Later.
   -- No compatibility check is performed
   procedure Merge (Former : in out Object; Later : in Object);
      
private

   package Element_Map is new Charles.Maps.Hashed.Strings.Unbounded (
      Element_Type, Charles.Hash_String, "=", Equal);

   type Object is new Element_Map.Container_Type;

   pragma Inline (Add_Word);

end Agpl.Simple_Dictionary;