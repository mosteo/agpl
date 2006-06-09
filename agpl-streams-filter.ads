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

--  Abstract class; A filter stream takes an access to a back stream from
--  which it can read/write, performing some extra operation/filtering if
--  needed.
--  This default implementation does nothing, just passing data along.

with Agpl.Streams.Controlled;

with Ada.Streams;

package Agpl.Streams.Filter is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Unknown_Availability : exception;
   --  Raised when we don't know in advance how many data is available.

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   --  Since it is derived from Controlled, Initialize and Finalize can be
   --  overriden
   type Stream_Type
   is new Agpl.Streams.Controlled.Stream_Type with private;

   type Stream_Access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The default creation procedure assigns the back filter to the stream.
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class);

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   --  Unimplemented for the default filter. (It hasn't caching capabilities).
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Write (This: in Stream_Type)
      return Stream_Element_Count;

private

   type Stream_Type
   is new Agpl.Streams.Controlled.Stream_Type with record
      Back      : Agpl.Streams.Stream_Access;
   end record;

end Agpl.Streams.Filter;
