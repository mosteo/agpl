------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
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
------------------------------------------------------------------------------

--  This task is to be still for some period of time.

with Agpl.Htn.Tasks.Primitive;

with Ada.Calendar; use Ada.Calendar;

package Agpl.Htn.Tasks.Still is

   pragma Elaborate_Body;

   type Object (<>) is new Tasks.Primitive.Object with private;
   type Object_Access is access Object'Class;

   function Create (Period : in Duration;
                    Start  : in Time := Clock) return Object;

   function Is_Still (This    : in Object;
                      Instant : in Time := Clock) return Boolean;
   --  Says if the time is in the still period

private

   type Object is new Tasks.Primitive.Object with
      record
         Start  : Time;
         Period : Duration;
      end record;

end Agpl.Htn.Tasks.Still;
