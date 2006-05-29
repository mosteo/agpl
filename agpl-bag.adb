------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
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
------------------------------------------------------------------------------

package body Agpl.Bag is

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context (This : in out Object; Context : in Bag_Context) is
   begin
      This.Context := Context;
   end Set_Context;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out Object;
                     Item : in     Item_type;
                     Pos  : in     Integer)
   is
   begin
      Insert (This, Item, Pos, Moving => null);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (This  : in out Object;
                     Item  : in     Item_type;
                     Pos   : in     Integer;
                     Moving : access procedure (Item    : in out Item_Type;
                                                Bag     : in out Bag_Context;
                                                Old_Pos,
                                                New_Pos : in     Integer))
   is
   begin
      if This.Busy then
         raise Program_Error;
      end if;

      if Pos > This.Last or else Pos < This.First then
         raise Constraint_Error;
      end if;

      if This.Last = This.Vector.all'Last then
         --  grow it!
         Grow (this);
      end if;

      This.Append (This.Vector (Pos));
      if Moving /= null then
         This.Busy := True;
         Moving (This.Vector (Last (This)), This.Context, Pos, Last (This));
         This.Busy := False;
      end if;

      This.Vector (Pos) := Item;
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Object; Pos : in Integer) is
   begin
      Delete (This, Pos, Moving => null);
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (This  : in out Object;
                     Pos   : in Integer;
                     Moving : access procedure (Item    : in out Item_Type;
                                                Bag     : in out Bag_Context;
                                                Old_Pos,
                                                New_Pos : in Integer))
   is
      Current_Last : constant Integer := This.Last;
   begin
      if This.Busy then
         raise Program_Error;
      end if;

      if Pos > This.Last or else Pos < This.First then
         raise Constraint_Error;
      end if;

      if Pos /= This.Last then
         This.Vector (Pos) := This.Vector (Last (This));
         if Moving /= null then
            This.Busy := True;
            Moving (This.Vector (Pos), This.Context, Last (This), Pos);
            This.Busy := False;
         end if;
      end if;

      if This.Last = Current_Last then
         Bag_Vectors.Object (This).Delete (This.Last);
      else
         --  Can only happen in Append was called within Move above!!
         This.Delete (Current_Last, Moving);
      end if;
   end Delete;

   ------------
   -- To_Bag --
   ------------

   function To_Bag (This : in Bag_Vectors.Item_Array) return Object is
      Result : Object (First => This'First);
   begin
      for I in This'Range loop
         Append (Result, This (I));
      end loop;

      return Result;
   end To_Bag;

end Agpl.Bag;
