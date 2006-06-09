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
--  $Id: agpl-constants.ads,v 1.1 2004/02/24 15:26:09 Jano Exp $

with Ada.Unchecked_Deallocation;

package body Agpl.Counter.Multi is

   use Counter_Map;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Safe_Object is
      --  Creates it if doesn't exists with value Increment
      procedure Add (Key : in String; Increment : in Integer := 1) is
         I : constant Cursor := Find (Values, Key);
         C : Counter.Object_Access;
      begin
         if I = No_Element then
            C := new Counter.Object;
            Insert (Values, Key, C);
         else
            C := Element (I);
         end if;
         C.Add (Increment);
      end Add;

      procedure Reset (Key : in String; Val     : in Integer := 0) is
         I : constant Cursor := Find (Values, Key);
         C : Counter.Object_Access;
      begin
         if I = No_Element then
            C := new Counter.Object;
            Insert (Values, Key, C);
         else
            C := Element (I);
         end if;
         C.Reset (Val);
      end Reset;

      function  Val (Key : in String) return Integer is
      begin
         return Element (Find (Values, Key)).Val;
      end Val;

      function  Max_Key return String is
         Max : Integer := Integer'First;
         Pos : Cursor := No_Element;
         I   : Cursor := First (Values);
      begin
         while I /= No_Element loop
            if Element (I).Val >= Max then
               Pos := I;
               Max := Element (I).Val;
            end if;
            Next (I);
         end loop;
         if Pos = No_Element then
            return "";
         else
            return Key (Pos);
         end if;
      end Max_Key;

      procedure Destroy is
         procedure Free is new Unchecked_Deallocation (
            Counter.Object, Counter.Object_Access);
         I   : Cursor := First (Values);
         Aux : Counter.Object_Access;
      begin
         while I /= No_Element loop
            Aux := Element (I);
            Free (Aux);
            Next (I);
         end loop;
      end Destroy;
   end Safe_Object;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin
      This.Safe.Destroy;
   end Finalize;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   --  Creates it if doesn't exists with value Increment
   procedure Add (This : in out Object; Key : in String; Increment : in Integer := 1) is
   begin
      This.Safe.Add (Key, Increment);
   end Add;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Key : in String; Val : in Integer := 0) is
   begin
      This.Safe.Reset (Key, Val);
   end Reset;

   ------------------------------------------------------------------------
   -- Val                                                                --
   ------------------------------------------------------------------------
   function  Val (This : in Object; Key : in String) return Integer is
   begin
      return This.Safe.Val (Key);
   end Val;

   ------------------------------------------------------------------------
   -- Max_Key                                                            --
   ------------------------------------------------------------------------
   function  Max_Key (This : in Object) return String is
   begin
      return This.Safe.Max_Key;
   end Max_Key;

end Agpl.Counter.Multi;
