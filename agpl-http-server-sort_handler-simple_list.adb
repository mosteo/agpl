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
--  $Id: agpl-http-server-sort_handler-simple_list.adb,v 1.2 2004/03/03 00:06:06 Jano Exp $

with Agpl.Http.Server.Sort_handler;
with Agpl.Http.Server.Sort_handler.Aux;

with Ada.Unchecked_deallocation;
use  Ada;
with System.Address_to_access_conversions;

package body Agpl.Http.Server.Sort_handler.Simple_list is

   type Object_access is access all Object;

   package Conv is new System.Address_to_access_conversions (Object);

   use Aux.Address_lists;
   
   Msgs    : Aux.Address_lists.Container_type;
   Pending : Natural := 0;
   pragma Atomic (Pending);

   protected Safe is
   procedure Add (This : in Object);
   procedure Clear;
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set);
   end Safe;

   protected body Safe is
   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object) is
      Auxp : Object_access;
      procedure Free is new Unchecked_deallocation (Object, Object_access);
   begin
      while Length (Msgs) >= Max_entries loop
         Auxp := Object_access (Conv.To_pointer (Element (Last (Msgs))));
         Free (Auxp);
         Delete_last (Msgs);
      end loop;
      Auxp := new Object'(This);
      Prepend (Msgs, Conv.To_address (Conv.Object_pointer (Auxp)));
      Pending := Pending + 1;
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Clear (Msgs);
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      use Agpl.Http.Server.Sort_handler;
      I    : Iterator_type := First (Msgs);
      Pos  : Positive      := 1;
   begin
      while I /= Back (Msgs) loop
         declare
            Row : Data_row;
            Q   : System.Address renames Element (I);
         begin
            Generate_row (
               Object_access (Conv.To_pointer (Q)).all, 
               Pos <= Pending, 
               Row);
            Append (Data, Row);
            I := Succ (I);
         end;
         Pos := Pos + 1;
      end loop;
      Pending := 0;
   end Http_report;
   end Safe;

   ------------------------------------------------------------------------
   -- New_events                                                         --
   ------------------------------------------------------------------------
   -- Says how many new events are there since last check.
   function New_events return Natural is
   begin
      return Pending;
   end New_events;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object) is
   begin
      Safe.Add (This);
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Safe.Clear;
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
   begin
      Safe.Http_report (Data);
   end Http_report;


end Agpl.Http.Server.Sort_handler.Simple_list;
