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

with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;

function Agpl.Htn.Plan_Node.Get_Random_Sibling (This : in Node_Access)
                                                return    Node_Access
is
   use Node_Vectors;
begin
   --  If this is not an OR node...
   if This.Parent = null or else Get_Kind (This.Parent) /= Or_Node then
      Log ("Plan_Node.Get_Random_Sibling: Parent node is " &
           Node_Kind'Image (Get_Kind (This.Parent)),
           Warning);
      return This;
   end if;

   return Element
     (This.Parent.Children,
      Random.Get_Integer (First_Index (This.Parent.Children),
        Last_Index  (This.Parent.Children)));
end Agpl.Htn.Plan_Node.Get_Random_Sibling;

