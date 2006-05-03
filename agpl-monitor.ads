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

--  Mutex with counter. A task may safely request it multiple times,
--  as long as it releases it the same times

with Ada.Finalization;
with Ada.Task_identification;

package Agpl.Monitor is

   pragma Elaborate_Body;

   use Ada;
   use Ada.Task_identification;

   Use_error : exception; -- Should never happen.

   protected type Semaphore is
      entry P;
      entry V;
   private
      entry Safe_P;

      Caller : Task_id := Null_task_id;           -- Requester
      In_use : Natural := 0;                      -- Times requested
   end Semaphore;

   type Semaphore_access is access all Semaphore;

   --  The following object is defined for conveniently usage of semaphores.
   --  Use:
   --  S : aliased Semaphore;
   --  declare
   --    M : Object (S'access);
   --  begin
   --    Exclusive_work;
   --  end;
   type Object (S : access Semaphore) is new
      Finalization.Limited_Controlled with null record;

   procedure Initialize (this : in out Object);
   procedure Finalize   (this : in out Object);

private

   pragma Inline (Initialize, Finalize);

end Agpl.Monitor;
