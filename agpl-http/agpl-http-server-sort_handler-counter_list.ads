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
--  $Id: aenea-countries.ads,v 1.2 2004/02/04 21:31:02 Jano Exp $

--  Keep track of items by key.

with Templates_parser;

generic
   Show_Negatives : Boolean := False;
package Agpl.Http.Server.Sort_Handler.Counter_List is

   pragma Elaborate_Body;

   No_More_Keys : exception;

   ------------------------------------------------------------------------
   -- Sum_key                                                            --
   ------------------------------------------------------------------------
   -- Add or remove to a key
   -- Use Inc => -1 to remove.
   procedure Sum_Key (Key : in String; Inc : in Integer := 1);

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   -- Elements under a given key
   function Count (Key : in String) return Integer;

   ------------------------------------------------------------------------
   -- Get_First_Key                                                      --
   ------------------------------------------------------------------------
   -- May raise No_More_Keys
   function Get_First_Key return String;

   ------------------------------------------------------------------------
   -- Get_Next_Key                                                       --
   ------------------------------------------------------------------------
   -- May raise No_More_Keys
   function Get_Next_Key (Current : in String) return String;

   -------------
   -- Iterate --
   -------------
   type Iterate_Code is access procedure (Key   : in String;
                                          Count : in Integer);

   procedure Iterate
     (Process   :    Iterate_Code;
      Negatives : in Boolean := False);
   -- Process will be called for each key/count pair

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Generates the data for the html report.
   -- Key, Count, %, Rounded %, Rounded, rounded (n / max)
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------------------------------------------------------------------
   -- Total_Count                                                        --
   ------------------------------------------------------------------------
   -- Sum of all key values
   function Total_Count return Integer;

   ------------------------------------------------------------------------
   -- Total_Report                                                       --
   ------------------------------------------------------------------------
   -- SINGLE1 <-- Sum of all key values
   function Total_Report return Templates_parser.Translate_table;

end Agpl.Http.Server.Sort_Handler.Counter_List;
