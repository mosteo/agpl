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

with Agpl.Trace;

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Agpl.Smart_Access is

   use Ada.Exceptions;

   ----------
   -- Free --
   ----------
   procedure Free is new
      Unchecked_deallocation (Item, Item_access);

   procedure Free is new
      Unchecked_deallocation (Tracker_type, Tracker_access);

   --------------
   -- Discount --
   --------------
   --  Helper decrementing function:
   procedure Discount (this : in out Tracker_access) is
   begin
      if This = null then
         raise Tracker_Already_Deallocated;
      end if;

      --  Special check for finalization when uninitialized.
      if This.Count > 0 then
         this.Count := this.Count - 1;
      end if;

      if this.Count = 0 then
         Free (this.Data); -- Could be null, no problem.
         Free (this);      -- Last one going out of scope.
      end if;
   end Discount;

   --------------
   -- Addcount --
   --------------
   --  Helper decrement function:
   procedure Addcount (this : in out Tracker_access) is
   begin
      this.Count := this.Count + 1;
   end Addcount;


   -------------
   -- Is_Null --
   -------------
   --  Is null?
   function Is_Null (this : in Object) return Boolean is
   begin
      return this.Tracker.Data = null;
   end Is_Null;

   ----------
   -- Bind --
   ----------
   --  Association:
   procedure Bind (this : in out Object; Data : in Item_access) is
      M : Monitor.Object (Mutex'Access);
      pragma Unreferenced (M);
   begin
      if This.Tracker.Data /= null then
         Raise_Exception (Allocated_Access'Identity, Item_Id);
      else
         this.Tracker.all := (Data => Data, Count => 1, Alloc => True);
      end if;
   end Bind;

   ----------
   -- Bind --
   ----------

   function Bind (This : in Item_Access) return Object is
      Result : Object;
   begin
      Bind (Result, This);
      return Result;
   end Bind;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (This  : in out Object;
      Data  : in     Item_Access;
      Force : in     Boolean := False)
   is
      M : Monitor.Object (Mutex'Access);
      pragma Unreferenced (M);
   begin
      if this.Tracker.Count = 1 or else Force then
         Free (this.Tracker.Data);
         This.Tracker.Data := Data;
      else
         Raise_Exception (Allocated_Access'Identity, Item_Id);
      end if;
   end Rebind;

   ------------
   -- Unbind --
   ------------
   --  Unbinding:
   --  The value is no longer controlled
   procedure Unbind (this : in out Object; Force : in Boolean := False) is
      M : Monitor.Object (Mutex'Access);
      pragma Unreferenced (M);
   begin
      if this.Tracker.Count = 1 or else Force then
         This.Tracker.all := (Data  => null,
                              Count => 0,
                              Alloc => True); -- So deallocation could be notified in Val.
         --  Free (this.Tracker.Data);
         --  This was like that for some time after migrating from Adagio to
         --  Agpl, but I'm 99% it was wrong.
      else
         Raise_Exception (Allocated_Access'Identity, Item_Id);
      end if;
   end Unbind;

   ---------
   -- Val --
   ---------
   --  Get value
   --  Of course, if the access value is copied outside, error can occur.
   function Val (this : in Object) return Item is
   begin
      if this.Tracker.Data /= null then
         return this.Tracker.Data.all;
      elsif This.Tracker.Alloc then
         Raise_Exception (Deallocated_Access'Identity, Item_Id);
      else
         Raise_Exception (Uninitialized_Access'Identity, Item_Id);
      end if;
   end Val;

   ---------
   -- Ref --
   ---------

   function Ref (this : in Object) return Item_access is
   begin
      return this.Tracker.Data;
   end Ref;

   ------------
   -- Adjust --
   ------------

   procedure Adjust   (this : in out Object) is
      M : Monitor.Object (Mutex'Access);
      pragma Unreferenced (M);
   begin
      Addcount (this.Tracker);
   exception
      when E : others =>
         Trace.Log ("Smart_Access " & Item_Id & " Adjust exception: " &
                    Trace.Report (E),
                    Trace.Error);
         raise;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (this : in out Object) is
      M : Monitor.Object (Mutex'Access);
      pragma Unreferenced (M);
   begin
      Discount (this.Tracker);
   exception
      when E : others =>
         Trace.Log ("Smart_Access " & Item_Id & " Finalize exception: " &
                    Trace.Report (E),
                    Trace.Error);
         raise;
   end Finalize;

   --  Can't work because the item is limited and can't be 'inputed

--     function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object
--     is
--        Valid : constant Boolean := Boolean'Input (S);
--        This  : Object;
--     begin
--        if Valid then
--           declare
--              Data : constant Item_Access := new Item'(Item'Input (S));
--           begin
--              This.Bind (Data);
--           end;
--        end if;
--        return This;
--     end Input;
--
--     procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
--                       This : in Object)
--     is
--     begin
--        Boolean'Output (S, Is_Null (This));
--        if not Is_Null (This) then
--           Item'Output (S, Val (This));
--        end if;
--     end Output;

end Agpl.Smart_Access;
