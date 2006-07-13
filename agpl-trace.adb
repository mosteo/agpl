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

--  Objects for tracing with disc dump optional.
--  This first implementation uses a protected object but not queuing.
--  Writing is done within the protected, which is theoretically illega.
--  Gnat's implementation of I/O allows it so in this first approach we'll
--  leave it like that.

with Agpl.Trace.Console;

with Ada.Containers.Vectors;

package body Agpl.Trace is

   package Tracer_Vectors is new Ada.Containers.Vectors
     (Positive, Object_Access);

   Stdout  : aliased Console.Object;

   Tracers : Tracer_Vectors.Vector;

   -- Safe logger --

   protected Safe is
      procedure Log (This    : in out Object'Class;
                     Text    : in     String;
                     Level   : in     Levels;
                     Section : in String := "");
   end Safe;

   protected body Safe is
      procedure Log (This    : in out Object'Class;
                     Text    : in     String;
                     Level   : in     Levels;
                     Section : in String := "")
      is
      begin
         This.Log (Text, Level, Section); -- Dispatch to the logger object.
      end Log;
   end Safe;

   ----------------
   -- Add_Tracer --
   ----------------

   procedure Add_Tracer (This : not null Object_Access) is
   begin
      --  We defer to post-elaboration the addition of the default tracer
      if Tracers.Is_Empty and then This /= Stdout'Access then
         Tracers.Append (Stdout'Access);
      end if;
      Tracers.Append (This);
   end Add_Tracer;

   --------------------
   -- Console_Tracer --
   --------------------

   function Console_Tracer return Object_Access is
   begin
      return Stdout'Access;
   end Console_Tracer;

   --------------------
   -- Enable_Section --
   --------------------

   procedure Enable_Section (Section : in String; Enabled : in Boolean := True)
   is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Enable_Section (Section, Enabled);
      end Do_It;
   begin
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Enable_Section;

   ---------
   -- Log --
   ---------
   --  In purpose, This can be null to allow the passing of Null_Object.
   procedure Log
     (This    : in     Object_Access;
      Text    : in     String;
      Level   : in     Levels;
      Section : in String := "") is
   begin
      if Enabled then
         if This /= null then
            Safe.Log (This.all, Text, Level, Section);
         end if;
      end if;
   end Log;


   ---------
   -- Log --
   ---------
   --  Logs to the default log object.
   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "") is
   begin
      if Enabled then
         --  We defer to post-elaboration the addition of the default tracer
         if Tracers.Is_Empty then
            Tracers.Append (Stdout'Access);
         end if;

         for I in Tracers.First_Index .. Tracers.Last_Index loop
            Safe.Log (Tracers.Element (I).all, Text, Level, Section);
         end loop;
      end if;
   end Log;

   -------------------
   -- Set_Decorator --
   -------------------

   procedure Set_Decorator (Decor : in Decorator) is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Set_Decorator (Decor);
      end Do_It;
   begin
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Set_Decorator;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (Level : in All_Levels) is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Set_Level (Level);
      end Do_It;
   begin
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Set_Level;

end Agpl.Trace;
