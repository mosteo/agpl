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

--  This class allows for a kind of "signaling" between an emitter and several
--  consumers.

--  Since there's no selectable signals for registration, it should not be used
--  for heavy traffic, I suppose.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Signal_Kind is (<>);
   type Message_Data (<>) is limited private;
package Agpl.Generic_Messenger is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   procedure Signal (This : in out Object;
                     Kind : in     Signal_Kind;
                     Data : in     Message_Data) is abstract;
   --  The emitter calls this function to notify all overriden descendents.
   --  A bidirectional communication must then ensue by means of @This@

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class);

   type Manager is tagged private;

   procedure Add_Listener (This : in out Manager; X : Object'Class);

   procedure Signal (This : in out Manager;
                     Kind : in     Signal_Kind;
                     Data : in     Message_Data);
   --  Will propagate the signal to all its managed objects.

private

   type Manager is tagged record
      Objects : Lists.List;
   end record;

end Agpl.Generic_Messenger;
