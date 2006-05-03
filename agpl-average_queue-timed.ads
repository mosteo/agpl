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

with Ada.Calendar;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
use  Ada;

generic
package Agpl.Average_queue.Timed is

   pragma Elaborate_Body;

   -- This object is thread safe:
   -- Number of averaging slots, and how many milliseconds these take:
   type Object (Slots : Positive := 12; Slot_duration : Positive := 5000)
   is limited private; 
   type Object_Access is access all Object;

   ------------------------------------------------------------------------
   -- Extended_Push                                                      --
   ------------------------------------------------------------------------
   -- Gives extra info: If a gap change has happened and how many empty
   -- gaps after it have happened:
   procedure Extended_Push (
      This       : in out Object;
      Data       : in     Item;
      Gap_Change : out Boolean;  -- True if at least a new gap has been pushed
      Empty_Gaps : out Natural); -- Number of empty gaps after the last one pushed
                                 -- and before the current one.
   
   ------------------------------------------------------------------------
   -- Push                                                               --
   ------------------------------------------------------------------------
   procedure Push (This : in out Object; Data : in Item);

   ------------------------------------------------------------------------
   -- Average                                                            --
   ------------------------------------------------------------------------
   procedure Average (This : in out Object; Result : out Item);

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access);

private 

   use type Calendar.Time;


   protected type Safe_object (Slots : Positive; Slot_duration : Positive) is
      procedure Push (Value  : in  Item; Gap_Change : out Boolean; Empty_Gaps : out Natural);
      procedure Avg  (Result : out Item);
      procedure Create;
      procedure Destroy;
   private
      -- Constant:
      Gap        : Duration      := Duration (Slot_duration) / 1000.0; 

      Slot_start : Calendar.Time := Calendar.Clock;
      Acum       : Item          := 0.0;
      Data       : Average_queue.Object_access;
   end Safe_object;

   type Object (Slots : Positive := 12; Slot_duration : Positive := 5000) is 
      new Finalization.Limited_Controlled with 
      record
         Safe       : Safe_object (Slots, Slot_duration);
      end record;

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

   pragma Inline (Push, Average);

end Agpl.Average_queue.Timed;
