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

--  Protected container for values of any private type.

with Ada.Finalization;

generic
   type Element_type (<>) is private;
package Agpl.Indefinite_Protected_Value is

   pragma Preelaborate;

   type Functor is abstract tagged null record;
   procedure Operate (This : in out Functor; Value : in out Element_Type)
   is abstract;

   type Object is limited private;

   type Object_Access is access Object;

   function Empty (This : in Object) return Boolean;
   --  Says if no value has been still stored.

   function Get (This : in Object) return Element_Type;

   procedure Set (This : in out Object; That : Element_Type);

   procedure Operate (This : in out Object; Using : in out Functor'Class);

private

   type Element_Access is access all Element_Type;

   protected type Internal_Object is
      function  Empty return Boolean;
      function  Get return Element_Type;
      procedure Set     (This  : in Element_Type);
      procedure Operate (Using : in out Functor'Class);
      --  Will call Functor.Operate (Value)
      procedure Free;
   private
      Value : Element_Access;
   end Internal_Object;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Internal : Internal_Object;
   end record;

   procedure Finalize (This : in out Object);

end Agpl.Indefinite_Protected_Value;
