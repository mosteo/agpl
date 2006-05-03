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
with DOM.Core.Nodes;

package Agpl.Xml is

   pragma Elaborate_Body;

   subtype Node is DOM.Core.Node;
   subtype Document is Node; --  NOT the document but the ROOT element.
   type Node_access is access all Node;
   type Document_access is access all Document;
   type Node_array is array (Integer range <>) of Node;

   Null_node : constant Node := null;

   Data_Error : exception;

   ------------------------------------------------------------------------
   -- From_String                                                        --
   ------------------------------------------------------------------------
   --  Parses and XML string (Latin1 accepted)
   function From_string (Data : in String) return Document;

   ------------------------------------------------------------------------
   -- Parse                                                              --
   ------------------------------------------------------------------------
   --  Read a XML file and stores it in memory;
   function Parse (File : String) return Document;

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   --  Retrieve the nth element in the hierarchy that matches the path.
   --  Separator is '/' Path example: "skins/html/default" Paths must be
   --  relatives to parent If unique is true, then an exception is raised if
   --  more than a value
   --    is found at the lower level.
   --  An exception is raised if any intermediate level has multiple matches.
   --  If ain't enough leaves, or doesn't exists some intermediate level,
   --    Null_node is returned.
   --  Null_node can be returned if some node along the path is non-existant.
   function Get
     (Path   : String;
      Parent : Node;
      Pos    : Positive := 1;
      Unique : Boolean  := False)
      return   Node;

   ------------------------------------------------------------------------
   -- Get_All                                                            --
   ------------------------------------------------------------------------
   --  Returns childrens with given name (first is 1): * means any name.
   function Get_all (Parent : Node; Name : String := "*") return Node_array;
   function Get_all (Path : String; Parent : Node) return Node_array;

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
   --  Here the attribute can be a proper Xml attribute or a child Element:
   --  <xml ada="X"/>
   --  <xml><ada>X</ada></xml>

   generic
      type Number is range <>;
   function Get_numeric_attribute_from_node
     (Item    : Node;
      Attr    : String;
      Default : Number)
      return          Number;

   function Get_attribute
     (Path    : String;
      Attr    : String;
      Parent  : Node;
      Default : String   := "";
      Pos     : Positive := 1;
      Unique  : Boolean  := False)
      return          String;
   --  Here the attribute can be a proper Xml attribute or a child Element:
   --  Get_Attribute ("Xml", "Ada") will return X in both these cases:
   --  <xml ada="X"/>
   --  <xml><ada>X</ada></xml>

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   --  Insertion functions:
   --  They return the inserted node.
   function Add (Parent : Node; Name : String) return Node;
   function Add
     (Parent : Node;
      Path   : String;
      Name   : String)
      return   Node;

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
