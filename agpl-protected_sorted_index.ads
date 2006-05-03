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
--  $Id: protected_sorted_index.ads,v 1.4 2004/01/21 21:05:47 Jano Exp $

--  Implements a double indexed container, with sorted semantics and
--  search by id.
--  However, for the search by Id to work, the ordering key must be the same too

with Ada.Containers.Ordered_Multisets;

generic
   type Element_type is private;
   with function "<" (
      Left : Element_type; Right : Element_type) return Boolean is <>;
   --  Orders Elements; usually will work on a part (the key) of an Element
   --  Elements will be ordered in ascending order according to "<"
   with function "=" (
      Left : Element_type; Right : Element_type) return Boolean is <>;
   --  Usually operates on part (the key) of an Element
package Agpl.Protected_sorted_index is

   pragma Preelaborate;

   package Implementation is new Ada.Containers.Ordered_Multisets (
      Element_type, "<", "=");

   protected type Sorted_index is

      procedure Clear;

      --  No duplicates (replacement)
      procedure Insert (Item : in Element_type);

      function Find (Item : in Element_type) return Boolean;

      --  Success will be true if the item has been found and deleted
      procedure Delete (Item : in Element_type; Success : out Boolean);

      --  Blocking if empty
      --  Doesn't remove it
      entry Get_first (Item : out Element_type);

      --  Get first if possible, without removing it:
      procedure Get_first (Item : out Element_type; Success : out Boolean);

      --  Get and remove an element if found, nothing else.
      procedure Get_remove (Item : in out Element_type; Found : out Boolean);

      --  Get first if exists and remove it
      procedure Get_first_remove (
         Item : out Element_type; Found : out Boolean);

      function Is_empty return Boolean;

      function Length return Natural;

   private

      List : Implementation.Set;

   end Sorted_index;

end Agpl.Protected_sorted_index;
