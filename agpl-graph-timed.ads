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

with Agpl.Average_Queue;
with Agpl.Average_Queue.Timed; pragma Elaborate_All (Agpl.Average_Queue.Timed);

with Ada.Finalization;
use  Ada;

package Agpl.Graph.Timed is

   --  pragma Elaborate_Body;

   --  Don't forget to check Agpl.Graph for all the functions available

   --  This timed graph doesn't create a new sample for each data added, but it groupes it
   --  optionally making a running average, and samples are added each time the specified
   --  gap elapses. Note that don't adding data will cause the graph to stall until some
   --  new data is added, and then it will fully actualize.
   --  Bug: when missing gaps are added, these aren't averaged. Instead zeroes are shown in
   --  the graph.

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  Sample_Averages is the number of samples that are averaged to make a sample (running average)
   --  Duration is given in milliseconds!
   --  It won't probably work for sample_durations under 1000
   type Object (Series, Samples, Sample_Averages, Sample_Duration : Positive) is
      new Graph.Object with private;

   ------------------------------------------------------------------------
   -- Add_sample                                                         --
   ------------------------------------------------------------------------
   --  Will not cause the addition of a sample like in Graph.Object, but the addition of data until
   --  enough time passes to cause the addition of a new sample.
   procedure Add_sample (This : in out Object; Serie : in Natural; Sample : in Float);

private

   package Avg_Float is new Agpl.Average_Queue (Float);
   package Avg       is new Avg_Float.Timed;

   type Controller (Parent : access Object) is new Finalization.Limited_Controlled
      with null record;

   type Avg_Array is array (Positive range <>) of Avg.Object_Access;

   type Object (Series, Samples, Sample_Averages, Sample_Duration : Positive) is new
   Graph.Object (Series => Series, Samples => Samples) with
      record
         Avgs : Avg_Array (1 .. Series);

         God  : Controller (Object'Access);
      end record;

   ------------------------------------------------------------------------
   -- Finalization                                                       --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Controller);
   procedure Finalize   (This : in out Controller);

end Agpl.Graph.Timed;
