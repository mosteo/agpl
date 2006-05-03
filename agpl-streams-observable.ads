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

--  An observable stream can tell in advance how many data you can read or
--  write into it.
--  Additionally, it is derived from Controlled so you could provide
--  initialization and finalization for it.

with Agpl.Streams.Controlled;

package Agpl.Streams.Observable is

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_Type 
   is abstract new Agpl.Streams.Controlled.Stream_Type with private;

   type Stream_Access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   function Available_Read (This : in Stream_Type) 
      return Stream_Element_Count is abstract;
   pragma Inline (Available_Read);

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   function Available_Write (This: in Stream_Type) 
      return Stream_Element_Count is abstract;
   pragma Inline (Available_Write);

private

   type Stream_Type 
   is abstract new Agpl.Streams.Controlled.Stream_Type with null record;

end Agpl.Streams.Observable;
