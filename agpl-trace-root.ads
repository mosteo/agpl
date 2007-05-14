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

--  Just implements sections but still doesn't trace.

with Agpl.Containers.String_Sets;

package Agpl.Trace.Root is

   pragma Preelaborate;

   type Object is limited new Trace.Object with private;
   type Object_Access is access all Object'Class;

   pragma Preelaborable_Initialization (Object);

   overriding
   procedure Log (This    : in out Object;
                  Text    : in     String;
                  Level   : in     Levels;
                  Section : in     String := "") is null;
   --  No need for implementations of this object to call Must_Log, it's called
   --  in the Agpl.Trace.Log subprogram.

   overriding
   function Must_Log (This    : in Object;
                      Level   : in Levels;
                      Section : in String) return Boolean;

   overriding
   procedure Enable_Section  (This    : in out Object;
                              Section : in     String;
                              Enabled : in     Boolean := True);

   overriding
   procedure Set_Active (This : in out Object; Active : in Boolean := True);

   overriding
   procedure Set_Level  (This : in out Object; Level : in All_Levels);

   overriding
   procedure Set_Decorator (This : in out Object; Decor : in Decorator);

   overriding
   function Decorate (This    : in Object;
                      Text    : in String;
                      Level   : in Levels;
                      Section : in String) return String;

private

   type Object is limited new Trace.Object with record
      Active   : Boolean    := True;
      Level    : All_Levels := Informative;
      Sections : Containers.String_Sets.Set;
      Decor    : Decorator;
   end record;

end Agpl.Trace.Root;
