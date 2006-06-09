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
--  $Id: agpl-graph.ads,v 1.2 2004/03/01 18:51:52 Jano Exp $

--  Type for generating a simple graph
--  Data values are floats, vertical scale is automatic, horizontal is fixed,
--  multiple series are allowed. Values in multiple series should be normali-
--  zed against a common scale.

--  Based in the Agpl.Average_Queue.Timed package, this allows to forget
--  about timing concerns. You push data into the graph and it will automa-
--  gically mix/average the data. The samples are added regularly as speci-
--  fied for the timed average queue.

with Ada.Unchecked_Deallocation;

with Text_IO;

package body Agpl.Graph.Timed is

   ------------------------------------------------------------------------
   -- Add_sample                                                         --
   ------------------------------------------------------------------------
   procedure Add_sample (This : in out Object; Serie : in Natural; Sample : in Float) is
      Gap_Change : Boolean;
      Empty_Gaps : Natural;
      Average    : Float;
   begin
      Avg.Extended_Push (This.Avgs (Serie).all, Sample, Gap_Change, Empty_Gaps);
      Text_IO.Put_Line (Gap_Change'Img);
      if Gap_Change then
         Avg.Average (This.Avgs (Serie).all, Average);
         Text_IO.Put_Line (Average'Img);
         Graph.Add_Sample (Graph.Object (This), Serie, Average);
         for I in 1 .. Empty_Gaps loop
            Text_IO.Put_Line ("Empty gap");
            Graph.Add_Sample (Graph.Object (This), Serie, 0.0);
         end loop;
      end if;
   end Add_Sample;

   ------------------------------------------------------------------------
   -- Finalization                                                       --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Controller) is
   begin
      for I in This.Parent.Avgs'Range loop
         This.Parent.Avgs (I) :=
            new Avg.Object (
               Slots         => This.Parent.Sample_Averages,
               Slot_Duration => This.Parent.Sample_Duration);
      end loop;
   end Initialize;

   procedure Finalize   (This : in out Controller) is
      procedure Free is new Unchecked_Deallocation (Avg.Object, Avg.Object_Access);
   begin
      for I in This.Parent.Avgs'Range loop
         Free (This.Parent.Avgs (I));
      end loop;
   end Finalize;

end Agpl.Graph.Timed;
