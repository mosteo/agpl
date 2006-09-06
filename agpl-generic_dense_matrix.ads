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

--  Dynamic large dense matrices, to avoid stack problems

private with Ada.Finalization;
private with Ada.Streams;

generic
   type Index_Type is range <>;
   type Cell_Type is private;
package Agpl.Generic_Dense_Matrix is

   pragma Preelaborate;

   type Matrix (<>) is tagged private;

   function Create (Last_Row,
                    Last_Col  : Index_Type;
                    First_Row,
                    First_Col : Index_Type := Index_Type'First)
                    return      Matrix;

   function Create (Last_Row,
                    Last_Col  : Index_Type;
                    Default   : Cell_Type;
                    First_Row,
                    First_Col : Index_Type := Index_Type'First)
                    return      Matrix;

   function First_Row (This : in Matrix) return Index_Type;
   function Last_Row  (This : in Matrix) return Index_Type;
   function First_Col (This : in Matrix) return Index_Type;
   function Last_Col  (This : in Matrix) return Index_Type;
   pragma Inline (First_Row, Last_Row, First_Col, Last_Col);

   function Rows (This : in Matrix) return Index_Type;
   function Cols (This : in Matrix) return Index_Type;
   pragma Inline (Rows, Cols);

   function Get (This : in Matrix; Row, Col : in Index_Type)
                 return    Cell_Type;

   function Ref (This : in Matrix; Row, Col : in Index_Type)
                 return    access Cell_Type;

   procedure Set (This : in out Matrix;
                  Row,
                  Col  : in     Index_Type;
                  Data : in     Cell_Type);

   pragma Inline (Get, Ref, Set);

private

   type Inner_Matrix is array (Index_Type range <>,
                               Index_Type range <>) of aliased Cell_Type;

   type Inner_Access is access Inner_Matrix;

   type Matrix is new Ada.Finalization.Controlled with record
      Ptr      : Inner_Access;
   end record;

   procedure Adjust   (This : in out Matrix);
   procedure Finalize (This : in out Matrix);

   use Ada.Streams;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      This   : out Matrix);

   for Matrix'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      This   :     Matrix);

   for Matrix'Write use Write;

end Agpl.Generic_Dense_Matrix;
