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

--  Xml related functions

with DOM.Core;
with Dom.Core.Nodes;
with Unicode.Ces;
with Unicode.Ces.Utf8;

with Ada.Containers.Vectors;

package Agpl.Xml is

   --  pragma Elaborate_Body;

   subtype Node is Dom.Core.Node;

   package Node_Vectors is new
     Ada.Containers.Vectors (Positive, Node, Dom.Core."=");

   subtype Document is Node; --  NOT the document but the ROOT element.
   type Node_access is access all Node;
   type Document_access is access all Document;
   type Node_Array is array (Integer range <>) of Node;
   subtype Node_Vector is Node_Vectors.Vector;

   Null_node : constant Node := null;

   Data_Error : exception;

   ------------------------------------------------------------------------
   -- From_String                                                        --
   ------------------------------------------------------------------------
   --  Parses and XML string (Latin1 accepted)
   function From_String
     (Data     : in String;
      Encoding : in Unicode.Ces.Encoding_Scheme := Unicode.Ces.Utf8.Utf8_Encoding)
      return        Document;

   ------------------------------------------------------------------------
   -- Parse                                                              --
   ------------------------------------------------------------------------
   --  Read a XML file and stores it in memory;
   function Parse (File     : String) return     Document;

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   --  Retrieve the named child. If it doesn exist, exception raised.
   function Get (Parent : Node; Name : String) return Node;
   function Get_Or_Null (Parent : Node; Name : String) return Node;
   --  Will return null instead of raising exception

   ------------------------------------------------------------------------
   -- Get_All                                                            --
   ------------------------------------------------------------------------
   --  Returns childrens with given name (first is 1): * means any name.
   function Get_all (Parent : Node; Name : String := "*") return Node_array;
   function Get_All (Path : String; Parent : Node) return Node_Array;

   function Get_all (Parent : Node; Name : String := "*") return Node_Vector;
   function Get_all (Path : String; Parent : Node) return Node_Vector;

   ------------------------------------------------------------------------
   -- Get_Attribute                                                      --
   ------------------------------------------------------------------------
   --  Read an attribute from a node: If not present and Default = "",
   --  Data_Error will be raised
   function Get_attribute
     (Item    : Node;
      Attr    : String;
      Default : String := "")
      return    String;

   generic
      type Number is range <>;
   function Get_numeric_attribute_from_node
     (Item    : Node;
      Attr    : String;
      Default : Number)
      return          Number;

   procedure Set_Attribute (Item : Node;
                            Attr : String;
                            Val  : String);

   --  Add a child by giving its name
   function Add (Parent : Node; Name : String) return Node;

   --  Add child node (must be for the same doc)
   procedure Add (Parent, Child : in Node);

   ------------------------------------------------------------------------
   -- Create_Child                                                       --
   ------------------------------------------------------------------------
   --  Creates a node for a document, without inserting it:
   function Create_Child (Parent : in Node; Name : in String) return Node;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   --  Deletion:
   --  Removes the Node from its location in a doc, and frees all memory
   --  Children are also removed.
   procedure Delete (Item : in out Node);

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_name (Item : Node) return String renames
     DOM.Core.Nodes.Node_Name;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   --  If no value and Default = "", Data_Error will be raised
   function Get_value
     (Item          : Node;
      Default : String := "")
      return          String;

   ------------------------------------------------------------------------
   -- Length                                                             --
   ------------------------------------------------------------------------
   --  This function returns the number of nodes found at the given level If
   --  some intermediate floor is multiple, then exception is raised.
   function Length (Path : String; Parent : Node) return Natural;

   ------------------------------------------------------------------------
   -- Length                                                             --
   ------------------------------------------------------------------------
   --  Returns the number of childs of a node with given name. * means any
   --  name.
   function Length (Parent : Node; Name : String := "*") return Natural;

   ------------------------------------------------------------------------
   -- Escape                                                             --
   ------------------------------------------------------------------------
   --  Takes a Latin1 string and encodes all invalid characters as &, <, etc.
   function Escape (This : in String) return String;

   ------------------------------------------------------------------------
   -- Unescape                                                           --
   ------------------------------------------------------------------------
   --  Restores an escaped string to its original form.
   function Unescape (This : in String) return String;

end Agpl.Xml;
