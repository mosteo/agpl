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

with Ada.Unchecked_Deallocation;

package body Agpl.Average_queue.Timed is

   ------------------------------------------------------------------------
   -- Extended_Push                                                      --
   ------------------------------------------------------------------------
   --  Gives extra info: If a gap change has happened and how many empty
   --  gaps after it have happened:

   procedure Extended_Push
     (This       : in out Object;
      Data       : in Item;
      Gap_Change : out Boolean;  -- True if at least a new gap has been pushed
      Empty_Gaps : out Natural)  -- Number of empty gaps after the last one
                                 --  pushed
   is
   begin
      This.Safe.Push (Data, Gap_Change, Empty_Gaps);
   end Extended_Push;

   ------------------------------------------------------------------------
   -- Push                                                               --
   ------------------------------------------------------------------------
   procedure Push (This : in out Object; Data : in Item) is
      Gap_Change : Boolean;
      Empty_Gaps : Natural;
   begin
      This.Safe.Push (Data, Gap_Change, Empty_Gaps);
   end Push;

   ------------------------------------------------------------------------
   -- Average                                                            --
   ------------------------------------------------------------------------
   procedure Average (This : in out Object; Result : out Item) is
   begin
      This.Safe.Avg (Result);
   end Average;

   ------------------------------------------------------------------------
   -- Safe_object                                                        --
   ------------------------------------------------------------------------
   protected body Safe_object is
      ----------
      -- Push --
      ----------
      procedure Push
        (Value      : in Item;
         Gap_Change : out Boolean;
         Empty_Gaps : out Natural)
      is
         Now : constant Calendar.Time := Calendar.Clock;
      begin
         if Now - Slot_start > Gap then
            --  Push acum
            Push (Data.all, Acum);
            Gap_Change := True;

            --  Zeroes for elapsed empty gaps
            Empty_Gaps :=
               Natural (Float'Floor
                           (Float ((Now - Slot_start - Gap) / Gap)));
            if Empty_Gaps >= Data.Size then
               for N in  1 .. Data.Size loop
                  Push (Data.all, 0.0);
               end loop;
            else
               for N in  1 .. Empty_Gaps loop
                  Push (Data.all, 0.0);
               end loop;
            end if;

            --  New acum:
            Acum := Value;

            --  New slot_start, the pushed one plus empty ones:
            Slot_start := Slot_start + Gap * (Empty_Gaps + 1);
         else
            Acum       := Acum + Value;
            Gap_Change := False;
            Empty_Gaps := 0;
         end if;
      end Push;

      ---------
      -- Avg --
      ---------
      procedure Avg (Result : out Item) is
         GC : Boolean; -- Out values, not used.
         EG : Natural; -- Out values, not used.
      begin
         Push (0.0, GC, EG); -- Update to current time
         if Is_empty (Data.all) then
            Result := 0.0;
         else
            Result := Average (Data.all) / Item (Gap);
         end if;
      end Avg;

      ------------
      -- Create --
      ------------
      procedure Create is
      begin
         Data := new Average_queue.Object (Size => Slots);
      end Create;

      -------------
      -- Destroy --
      -------------
      procedure Destroy is
         procedure Free is new Unchecked_Deallocation (
            Average_queue.Object'Class,
            Average_queue.Object_access);
      begin
         Free (Data);
      end Destroy;

   end Safe_object;

   procedure Initialize (This : in out Object) is
   begin
      This.Safe.Create;
   end Initialize;

   procedure Finalize (This : in out Object) is
   begin
      This.Safe.Destroy;
   end Finalize;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access) is
      procedure Delete is new Ada.Unchecked_Deallocation (
         Object,
         Object_Access);
   begin
      Delete (This);
   end Free;

end Agpl.Average_queue.Timed;
