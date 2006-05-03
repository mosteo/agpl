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
--  $Id: average_queue.adb,v 1.3 2004/01/21 21:05:43 Jano Exp $

package body Agpl.Average_queue is

   ----------
   -- Push --
   ----------

   procedure Push (this: in out Object; New_item: in Item) is
   begin
      This.Sum := This.Sum + New_Item;

      if This.Length >= This.Data'Length then
         --  Remove item to be dropped from acummulator:
         This.Sum := This.Sum - This.Data (This.Pos);
      end if;

      This.Data (This.Pos) := New_Item;
      This.Pos             := This.Pos + 1;

      if This.Pos > This.Data'Last then
         This.Pos := This.Data'First;
      end if;

      if This.Length < This.Data'Length then
         This.Length := This.Length + 1;
      end if;
   end Push;

   -------------
   -- Average --
   -------------

   function Average (this: in Object) return Item is
   begin
      if this.Length = 0 then
         raise No_data;
      else
         return This.Sum / Item (This.Length);
      end if;
   end Average;

   --------------
   -- Is_empty --
   --------------

   function Is_empty (This : in Object) return Boolean is
   begin
      return This.Length = 0;
   end Is_empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : out Object) is
   begin
      This.Length := 0;
      This.Pos    := 1;
      This.Sum    := 0.0;
   end Clear;

end Agpl.Average_queue;
