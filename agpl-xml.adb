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

with Agpl.Strings;
with Agpl.Strings.Fields; use Agpl.Strings.Fields;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.File;
with Input_Sources.Strings;
with Sax.Readers;
with Unicode.CES.Basic_8bit; use Unicode.CES;

with Ada.Exceptions; use Ada.Exceptions;
with Text_IO;

package body Agpl.Xml is

   package DCD renames DOM.Core.Documents;
   package DCE renames DOM.Core.Elements;
   package DCN renames DOM.Core.Nodes;

   use type DOM.Core.Node;
   use type DOM.Core.Node_Types;

   Unsafe_Chars : constant array (Character) of Boolean :=
      ('&' | ''' | '"' | '<' | '>' => True,
      others => False);

   type String_Access is access String;
   Replacings : constant array (Character) of String_Access :=
      ('&'   => new String'("&amp;"),
      '''    => new String'("&apos;"),
      '"'    => new String'("&quot;"),
      '<'    => new String'("&lt;"),
      '>'    => new String'("&gt;"),
      others => new String'(""));

   function L (this : in String) return String renames Agpl.Strings.To_lower;

   ------------------------------------------------------------------------
   -- From_string                                                        --
   ------------------------------------------------------------------------
   --  Parses and XML string (Latin1 accepted)
   function From_String (Data : in String) return Document is
      Tree          : DOM.Readers.Tree_Reader;
      String_handle : Input_Sources.Strings.String_Input;
      N             : Node;
   begin
      --  Needed for namespaces:
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.Namespace_Prefixes_Feature,
         True);

      Input_Sources.Strings.Open
        (Data'Unrestricted_Access,
         Basic_8bit.Basic_8bit_Encoding,
         String_handle);
      DOM.Readers.Parse (Tree, String_handle);
      Input_Sources.Strings.Close (String_handle);
      N := DCD.Get_Element (DOM.Readers.Get_Tree (Tree));
      DCN.Normalize (N);
      return N;
   end From_String;

   ------------------------------------------------------------------------
   -- Parse                                                              --
   ------------------------------------------------------------------------
   --  Read a XML file and stores it in memory;
   function Parse (File : String) return Document is
      Tree        : DOM.Readers.Tree_Reader;
      File_handle : Input_Sources.File.File_Input;
      N           : Node;
   begin
      --  Needed for namespaces:
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.Namespace_Prefixes_Feature,
         True);

      Input_Sources.File.Open (File, File_handle);
      Input_Sources.File.Set_Encoding
        (File_handle,
         Basic_8bit.Basic_8bit_Encoding);
      DOM.Readers.Parse (Tree, File_handle);
      Input_Sources.File.Close (File_handle);
      N := DCD.Get_Element (DOM.Readers.Get_Tree (Tree));
      DCN.Normalize (N);
      return N;
   exception
      when E : others =>
         Text_IO.Put_Line ("Syntax error in XML file: " & File);
         Text_IO.Put_Line (Exception_Information (E));
         raise;
   end Parse;

   ---------
   -- Get --
   ---------
   function Get
     (Path   : String;
      Parent : Node;
      Pos    : Positive := 1;
      Unique : Boolean  := False)
      return   Node
   is
      Head : constant String := String_Head (Path);
      Tail : constant String := String_Tail (Path);
   begin
      if Path = "" then
         return Parent;
      end if;
      if Parent.Node_Type /= DOM.Core.Element_Node then
         raise Storage_Error;
      end if;
      declare
         Nodes : constant Node_array := Get_all (Parent, Head);
      begin
         if Tail = "" then -- Lower level reached
            if Unique and then Nodes'Length > 1 then
               raise Constraint_Error;
            elsif Nodes'Length < Pos then
               return Null_node;
            else
               return Nodes (Pos);
            end if;
         end if;

         --  Whe should continue descending the tree if possible:
         if Nodes'Length = 0 then
            return Null_node;
         elsif Nodes'Length > 1 then
            raise Constraint_Error;
         end if;

         --  Recursive call:
         return Get (Tail, Nodes (1), Pos, Unique);
      end;
   end Get;

   -------------
   -- Get_All --
   -------------
   --  Returns childrens with given name (first is 1):
   --  * means any name.
   function Get_all (Parent : Node; Name : String := "*") return Node_array is
      Num      : Natural := 0;
      Children : DOM.Core.Node_List;
   begin
      if Parent = Null_node then
         return Node_array'((1 .. 0 => Null_node));
      end if;
      --  Let's see how many children this node has:
      Children := DCN.Child_Nodes (Parent);
      for n in  0 .. DCN.Length (Children) - 1 loop
         if DCN.Item (Children, n).Node_Type = DOM.Core.Element_Node
           and then (Name = "*"
                    or else L (DCN.Node_Name (DCN.Item (Children, n))) =
                            L (Name))
         then
            Num := Num + 1;
         end if;
      end loop;
      --  Now let's create the vector and return it:
      declare
         Result : Node_array (1 .. Num);
         pos    : Positive := 1;
         Item   : Node;
      begin
         for n in  0 .. DCN.Length (Children) - 1 loop
            Item := DCN.Item (Children, n);
            if Item.Node_Type = DOM.Core.Element_Node
              and then (Name = "*"
                       or else L (DCN.Node_Name (Item)) = L (Name))
            then
               Result (pos) := Item;
               pos          := pos + 1;
            end if;
         end loop;
         return Result;
      end;
   end Get_all;

   function Get_all (Path : String; Parent : Node) return Node_array is
      New_parent : Node;
   begin
      if Parent = Null_node then
         return Node_array'((1 .. 0 => Null_node));
      end if;
      if Path = "" then
         return Node_array'((1 => Parent));
      end if;
      declare
         Head : constant String := String_Tail_Reverse (Path);
         Tail : constant String := String_Head_Reverse (Path);
      begin
         if Head = "" then
            return Get_all (Parent, Tail);
         else
            New_parent := Get (Head, Parent, Unique => True);
            if New_parent = Null_node then
               return Node_array'((1 .. 0 => Null_node));
            else
               return Get_all (New_parent, Tail);
            end if;
         end if;
      end;
   end Get_all;

   ------------------------------------------------------------------------
   -- Get_Attribute                                                      --
   ------------------------------------------------------------------------
   --  Read an attribute from a node:
   function Get_attribute
     (Item          : Node;
      Attr          : String;
      Default : String := "")
      return          String
   is
   begin
      declare
         Result : constant String := DCE.Get_Attribute (Item, Attr);
      begin
         if Result /= "" then
            return Result;
         end if;
         --  Let's try to get a child element with value as attr:
         declare
            Child : constant Node := Get (Attr, Item, Unique => True);
         begin
            return Get_value (Child, Default);
         end;
      end;
   exception
      when others =>
         if Default /= "" then
            return Default;
         else
            raise Data_Error;
         end if;
   end Get_attribute;

   function Get_numeric_attribute_from_node
     (Item          : Node;
      Attr          : String;
      Default : Number)
      return          Number
   is
   begin
      return Number'Value
               (Get_attribute (Item, Attr, Number'Image (Default)));
   exception
      when others =>
         return Default;
   end Get_numeric_attribute_from_node;

   function Get_attribute
     (Path          : String;
      Attr          : String;
      Parent        : Node;
      Default : String   := "";
      Pos           : Positive := 1;
      Unique        : Boolean  := False)
      return String is
      Item : Node;
   begin
      if Parent = null then
         if Default /= "" then
            return Default;
         else
            raise Data_Error;
         end if;
      end if;
      Item := Get (Path, Parent, Pos, Unique);
      declare
         Result : constant String := Get_attribute (Item, Attr, Default);
      begin
         return Result;
      end;
   exception
      when Data_Error =>
         raise;
      when others =>
         if Default /= "" then
            return Default;
         else
            raise Data_Error;
         end if;
   end Get_attribute;


   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   --  Insertion functions:
   --  They return the inserted node.
   function Add (Parent : Node; Name : String) return Node is
   begin
      return DCN.Append_Child
               (Parent,
                DCD.Create_Element (DCN.Owner_Document (Parent), Name));
   end Add;

   function Add
     (Parent : Node;
      Path   : String;
      Name   : String)
      return   Node
   is
      Inmediate_parent : Node;
   begin
      Inmediate_parent := Get (Path, Parent, Unique => True);
      return Add (Inmediate_parent, Name);
   end Add;

   --  Add child node (must be for the same doc)
   procedure Add (Parent, Child : in Node) is
      Dummy : constant Node := DCN.Append_Child (Parent, Child);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Add;

   ------------------------------------------------------------------------
   -- Create_Child                                                       --
   ------------------------------------------------------------------------
   --  Creates a node for a document, without inserting it:
   function Create_Child (Parent : in Node; Name : in String) return Node is
   begin
      return DCD.Create_Element (DCN.Owner_Document (Parent), Name);
   end Create_Child;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   procedure Delete (Item : in out Node) is
      Dummy : Node;
   begin
      --  If it's the root element, we free everything:
      if DCD.Get_Element (DCN.Owner_Document (Item)) = Item then
         Dummy := DCN.Owner_Document (Item);
      else
         Dummy := DCN.Remove_Child (DCN.Parent_Node (Item), Item);
      end if;
      DCN.Free (Dummy, Deep => True);
      Item := null;
   end Delete;

   ------------------------------------------------------------------------
   -- Length                                                             --
   ------------------------------------------------------------------------
   --  This function returns the number of nodes found at the given level
   function Length (Path : String; Parent : Node) return Natural is
   begin
      if Path = "" then
         return 1;
      else
         declare
            Nodes : constant Node_array := Get_all (Path, Parent);
         begin
            return Nodes'Length;
         exception
            when Constraint_Error =>
               return 0;
         end;
      end if;
   end Length;

   function Length (Parent : Node; Name : String := "*") return Natural is
   begin
      if Name = "" then
         raise Constraint_Error;
      end if;
      declare
         Nodes : constant Node_array := Get_all (Parent, Name);
      begin
         return Nodes'Length;
      exception
         when Constraint_Error =>
            return 0;
      end;
   end Length;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   function Get_value (Item : Node; Default : String := "") return String
   is
      Nodes : constant DOM.Core.Node_List := DCN.Child_Nodes (Item);
   begin
      if DCN.Length (Nodes) = 0 then
         if Default = "" then
            raise Data_Error;
         else
            return Default;
         end if;
      end if;
      for n in  0 .. DCN.Length (Nodes) - 1 loop
         if DCN.Item (Nodes, n).Node_Type = DOM.Core.Text_Node then
            return DCN.Node_Value (DCN.Item (Nodes, n));
         end if;
      end loop;
      if Default /= "" then
         return Default;
      else
         raise Data_Error;
      end if;
   end Get_value;

   ------------------------------------------------------------------------
   -- Escape                                                             --
   ------------------------------------------------------------------------
   --  Takes a Latin1 string and encodes all invalid characters as &, <, etc.
   function Escape (This : in String) return String is
      Aux : UString  := U (This);
      Pos : Positive := 1;
      Cur : Character;
   begin
      loop
         exit when Pos > ASU.Length (Aux);

         Cur := ASU.Element (Aux, Pos);
         if Unsafe_Chars (Cur) then
            ASU.Replace_Slice (Aux, Pos, Pos, Replacings (Cur).all);
         end if;

         Pos := Pos + 1;
      end loop;

      return S (Aux);
   end Escape;

   ------------------------------------------------------------------------
   -- Unescape                                                           --
   ------------------------------------------------------------------------
   --  Restores an escaped string to its original form.
   function Unescape (This : in String) return String is
      Aux : UString := U (This);
      Pos : Natural;
   begin
      for I in  Unsafe_Chars'Range loop
         if Unsafe_Chars (I) then
            loop
               Pos := ASU.Index (Aux, Replacings (I).all);
               exit when Pos < 1;
               ASU.Replace_Slice
                 (Aux,
                  Pos,
                  Pos + Replacings (I)'Length - 1,
                  "" & I);
            end loop;
         end if;
      end loop;

      return S (Aux);
   end Unescape;

end Agpl.Xml;