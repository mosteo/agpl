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

--  Auxiliary type to index by agent x task

with Agpl.Cr.Agent;
with Agpl.Htn.Tasks;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers;

package Agpl.Cr.Agent_Task_Key is

   pragma Elaborate_Body;

   type Object (<>) is private;

   function Get (A : in Cr.Agent.Object'Class;
                 T : in Htn.Tasks.Object'Class) return Object;

   function Hash (This : in Object) return Ada.Containers.Hash_Type;

private

   type Object is record
      Agent   : Ustring;
      Tid     : Htn.Tasks.Task_Id;
   end record;


end Agpl.Cr.Agent_Task_Key;
