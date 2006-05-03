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

package body Agpl.Monitor is

   protected body Semaphore is

      entry P when True is
      begin
         if Caller = P'Caller then
            In_use := In_use + 1;
         elsif In_use = 0 then
            Caller := P'Caller;
            In_use := 1;
         else
            requeue Safe_P with abort;
         end if;
      end P;

      entry Safe_P when In_use = 0 is
      begin
         Caller := Safe_P'Caller;
         In_use := 1;
      end Safe_P;

      entry V when True is
      begin
         if V'Caller /= Caller then
            raise Use_error;
         else
            In_use := In_use - 1;
            if In_use = 0 then
               Caller := Null_task_id;
            end if;
         end if;
      end V;

   end Semaphore;

   ----------------
   -- Initialize --
   ----------------
   --  Get
   procedure Initialize(this: in out Object) is
   begin
      this.S.P;
   end Initialize;


   --------------
   -- Finalize --
   --------------
   --  Release
   procedure Finalize(this: in out Object) is
   begin
      this.S.V;
   end Finalize;

end Agpl.Monitor;
