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

with Agpl.Exceptions;

package body Agpl.Streams.Filter is

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The default creation procedure assigns the back filter to the stream.
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class) is
   begin
      This.Back := Agpl.Streams.Stream_Access (Back);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Ada.Streams.Read (This.Back.all, Item, Last);
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Streams.Write (This.Back.all, Item);
   end Write;

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   --  Unimplemented for the default filter. (It hasn't caching capabilities).
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Count);
   begin
      raise Exceptions.Unimplemented;
   end Prefetch;

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count
   is
   pragma Unreferenced (This);
   begin
      return Stream_Element_Count'Last;
   end Available_Read;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Write (This: in Stream_Type)
      return Stream_Element_Count is
      pragma Unreferenced (This);
   begin
      return Stream_Element_Count'Last;
   end Available_Write;

end Agpl.Streams.Filter;
