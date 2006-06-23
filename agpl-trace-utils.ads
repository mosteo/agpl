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
--  $Id: agpl.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

--  Objects for tracing with disc dump optional. This first implementation uses
--  a protected object but not queuing. Writing is done within the protected,
--  which is theoretically illega. Gnat's implementation of I/O allows it so in
--  this first approach we'll leave it like that.

package Agpl.Trace.Utils is

   pragma Elaborate_Body;

   --  Prependers:

   function Prepend_Level (Text    : in String;
                           Level   : in Levels;
                           Section : in String) return String;
   --  Adds a marker of message level
   pragma Inline (Prepend_Level);

   function Prepend_Timestamp (Text    : in String;
                               Level   : in Levels;
                               Section : in String) return String;
   --  Add a timestamp.
   pragma Inline (Prepend_Timestamp);

   function Prepend_Level_Timestamp (Text    : in String;
                                     Level   : in Levels;
                                     Section : in String) return String;
   --  Add level & timestamp
   pragma Inline (Prepend_Level_Timestamp);

end Agpl.Trace.Utils;
