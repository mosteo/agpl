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
--  $Id: average_queue.ads,v 1.3 2004/01/21 21:05:43 Jano Exp $

generic
   type Item is digits <>;
package Agpl.Average_queue is

   pragma Preelaborate;

   --  Raised when no elements to average:
   No_data : exception;

   type Object (Size: Positive) is tagged private;
   type Object_access is access all Object'Class;

   --  Add a new item
   --  Will lost olders when size exceeded
   procedure Push (this: in out Object; New_item: in Item);

   --  Returns the average of pushed objects
   --  May raise No_data
   function Average (this: in Object) return Item;

   --  Says if there is no data to average.
   function Is_empty (This : in Object) return Boolean;

   --  Clear contents
   procedure Clear (This : out Object);

private

   type Data_array is array(Positive range <>) of Item;

   type Object (Size: Positive) is tagged record
      Data   : Data_array (1 .. Size);
      Length : Natural  := 0;
      Pos    : Positive := 1;
      Sum    : Item     := 0.0;
      --  Cached sum to avoid computation when requesting average.
   end record;

end Agpl.Average_queue;
