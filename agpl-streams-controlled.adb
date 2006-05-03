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
--  $Id: agpl-streams-circular.ads,v 1.1 2004/03/22 07:14:59 Jano Exp $

--  A stream which has a finalization and initialization method.

package body Agpl.Streams.Controlled is

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   --  Called on destruction of the stream
   --  Default one does nothing.
   procedure Finalize (This : in out Stream_Type) is
      pragma Unreferenced (This);
   begin
      null;
   end Finalize;

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   --  Called on creation of the stream
   --  Default one does nothing.
   procedure Initialize (This : in out Stream_Type) is
      pragma Unreferenced (This);
   begin
      null;
   end Initialize;

   procedure Initialize (This : in out Controller_Type) is
   begin
      Initialize (This.Parent.all);
   end Initialize;

   procedure Finalize   (This : in out Controller_Type) is
   begin
      Finalize (This.Parent.all);
   end Finalize;

end Agpl.Streams.Controlled;
