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
--  $Id: event_queue.ads,v 1.6 2004/01/29 21:47:09 Jano Exp $

--  Efficient event queue. Useful for timeouts, as an example.

--  Implemented with tagged types. That makes genericity unnecesary. A queue
--   can perform multiple kind of events.

package Agpl.Event_queues is

   pragma Preelaborate;

   --  This type can be extended to match any required context
   type Context_type is tagged null record;
   type Context_access is access all Context_type'Class;
--   for Context_access'Storage_pool use Adagio.Debug_pool;

   --  Callback function called when event triggers
   type Action_procedure is access procedure (Context : in Context_access);

   type Master_States is (Waiting_Worker, Waiting_Deadline, Executing, Idle, Ready);
   type Worker_States is (Waiting, Executing);

private

   type Action_type is (New_event, Job_finished);

end Agpl.Event_queues;
