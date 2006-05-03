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

with Ada.Unchecked_Deallocation;

package body Agpl.Indefinite_Protected_Value is

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   -----------
   -- Empty --
   -----------

   function Empty (This : in Object) return Boolean is
   begin
      return This.Internal.Empty;
   end Empty;

   ---------
   -- Get --
   ---------

   function Get (This : in Object) return Element_Type is
   begin
      return This.Internal.Get;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Object; That : Element_Type) is
   begin
      This.Internal.Set (That);
   end Set;

   -------------
   -- Operate --
   -------------

   procedure Operate (This : in out Object; Using : in out Functor'Class) is
   begin
      This.Internal.Operate (Using);
   end Operate;

   ---------------------
   -- Internal_Object --
   ---------------------

   protected body Internal_Object is

      -----------
      -- Empty --
      -----------

      function Empty return Boolean is
      begin
         return Value = null;
      end Empty;

      ---------
      -- Get --
      ---------

      function Get return Element_Type is
      begin
         return Value.all;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (This  : in Element_Type) is
      begin
         Free (Value);
         Value := new Element_Type'(This);
      end Set;

      -------------
      -- Operate --
      -------------

      procedure Operate (Using : in out Functor'Class) is
      begin
         Operate (Using, Value.all);
      end Operate;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         Free (Value);
      end Free;

   end Internal_Object;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      This.Internal.Free;
   end Finalize;

end Agpl.Indefinite_Protected_Value;
