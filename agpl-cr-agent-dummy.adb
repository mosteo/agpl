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

--  This agent doesn't know how to do anything, but can be used as a placeholder
--  is Assignation objects :/

package body Agpl.Cr.Agent.Dummy is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This     : in out Object;
      The_Task : in out Htn.Tasks.Primitive.Object'Class;
      Plan     : in out Htn.Plan.Object;
      Done     :    out Boolean)
   is
      pragma Unreferenced (This, The_Task, Plan);
   begin
      Done := False;
   end Execute;

end Agpl.Cr.Agent.Dummy;
