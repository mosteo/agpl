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
--  As a special exception, if other files instantiate generics from        --
--  this unit, or you link this unit with other files to produce an         --
--  executable, this unit does not by itself cause the resulting            --
--  executable to be covered by the GNU General Public License. This        --
--  exception does not however invalidate any other reasons why the         --
--  executable file might be covered by the GNU Public License.             --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: dom-core-nodes-output.adb,v 1.3 2004/01/21 21:05:43 Jano Exp $

with Agpl.Types.Ustrings;
with Agpl.Xml;

with Sax.Encodings; use Sax.Encodings;

with Ada.Strings.Unbounded.Text_Io;
with Ada.Text_Io;

package body Dom.Core.Nodes.Output is

   NL  : constant String  := (1 => Character'Val (10));
   Tab : constant Natural := 3;

   procedure Print_Whites (U : in out ASU.Unbounded_String; Whites : in Natural) is
      S : constant String (1 .. Whites) := (others => ' ');
   begin
      ASU.Append (U, S);
   end Print_Whites;

   ----------
   -- Sort --
   ----------

   procedure Sort (Map : in out Named_Node_Map) is
      Arr : Node_Array (0 .. Map.Last + 1) := (others => null);
      Index : Natural;
   begin
      --  ??? The algorithm is not efficient, we use Insertion_Sort.
      for J in 0 .. Map.Last loop
         Index := 0;
         loop
            if Arr (Index) = null then
               Arr (Index) := Map.Items (J);
               exit;
            end if;

            if Node_Name (Map.Items (J)) <= Node_Name (Arr (Index)) then
               Arr (Index + 1 .. Arr'Last) := Arr (Index .. Arr'Last - 1);
               Arr (Index) := Map.Items (J);
               exit;
            end if;
            Index := Index + 1;
         end loop;
      end loop;
      for J in 0 .. Map.Last loop
         Map.Items (J) := Arr (J);
      end loop;
   end Sort;

   -----------
   -- Print --
   -----------
   procedure Print
     (N              : Node;
      U              : in out ASU.Unbounded_string;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0)
   is
   --  Print the contents of Node and its children in XML format.
   --  If Print_Comments is True, then nodes associated with comments are
   --  also displayed.
   --  The <?xml?> processing instruction is displayed only if Print_XML_PI
   --  By default, names are of the form  ns_prefix:local_name. However, if
   --  with_URI is True, names will be  ns_URI:local_name instead
      procedure Append (U : in out ASU.Unbounded_string; S : String)
         renames ASU.Append;
      procedure Print_Name (N : Node);
      --  Print the name of the node.

      ----------------
      -- Print_Name --
      ----------------

      procedure Print_Name (N : Node) is
      begin
         if With_URI then
            Append (U, Namespace_URI (N) & ':' & Agpl.Xml.Escape (Local_Name (N)));
         else
            Append (U, Agpl.Xml.Escape (Node_Name (N)));
         end if;
      end Print_Name;

   begin
      if N = null then
         return;
      end if;

      case N.Node_Type is
         when Element_Node =>
            Append (U, NL);
            Print_Whites (U, Indent);
            Append (U, "<");
            Print_Name (N);

            --  Sort the XML attributes as required for canonical XML
            Sort (N.Attributes);

            if N.Attributes.Last > 0 then
               for J in 0 .. N.Attributes.Last loop
                  Append (U, " ");
                  Print (N.Attributes.Items (J), U,
                         Print_Comments, Print_XML_PI, With_URI, Indent => Indent + Tab);
               end loop;
            elsif N.Attributes.Last = 0 then
               Append (U, " ");
               Print_Name (N.Attributes.Items (0));
               Append (U, "=""");
               Append (U, Node_Value (N.Attributes.Items (0)));
               Append (U, """");
            end if;

            Append (U, ">");

            if N.Children.Last >= 0 then
               Print (N.Children, U, Print_Comments, Print_XML_PI, With_URI, Indent => Indent + Tab);
            end if;

            if N.Attributes.Last >= 1 or else
               (N.Children.Last >= 0 and then N.Children.Items (N.Children.Last).Node_Type /= Text_Node) then
               Append (U, NL);
               Print_Whites (U, Indent);
            end if;

            Append (U, "</");
            Print_Name (N);
            Append (U, ">");

         when Attribute_Node =>
            Append (U, NL);
            Print_Whites (U, Indent);
            Print_Name (N);
            Append (U, "=""");
            Append (U, Agpl.Xml.Escape (Node_Value (N)));
            Append (U, """");

         when Processing_Instruction_Node =>
            if Print_XML_PI
              or else N.Target.all /= Xml_Sequence
            then
               Append (U, "<?" & N.Target.all);
               if N.Pi_Data'Length = 0
                 or else N.Pi_Data (N.Pi_Data'First) /= ' '
               then
                  Append (U, " ");
               end if;
               Append (U, N.Pi_Data.all & "?>");
            end if;

         when Comment_Node =>
            if Print_Comments then
               Append (U, Node_Value (N));
            end if;

         when Document_Node =>
            Print (N.Doc_Children, U,
                   Print_Comments, Print_XML_PI, With_URI);

         when Document_Fragment_Node =>
            Print (N.Doc_Frag_Children, U,
                   Print_Comments, Print_XML_PI, With_URI);

         when Document_Type_Node | Notation_Node =>
            null;

         when Text_Node =>
            Append (U, Node_Value (N));

         when others =>
            Append (U, Node_Value (N));
      end case;

      --  Lame hack:
      declare
         use Asu;
      begin
         if Slice (U, 1, 2) /= "<?" then
            U := "<?xml version='1.0' encoding='utf-8'?>" & Nl & U;
         end if;
      end;
   end Print;

   procedure Print
     (N              : Node;
      File           : String;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0)
   is
      use Agpl.Types.Ustrings;
      Text : Ustring;
   begin
      Print (N, Text, Print_Comments, Print_Xml_Pi, With_Uri, Indent);

      declare
         use Ada.Strings.Unbounded.Text_Io;
         use Ada.Text_Io;
         F : File_Type;
      begin
         Create (F, Out_File, File);
         Put_Line (F, Text);
         Close (F);
      exception
         when others =>
            Close (F);
            raise;
      end;
   end Print;

   procedure Print
     (List           : Dom.Core.Node_List;
      U              : in out ASU.Unbounded_string;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0)
   is
   --  Same as Print, but for all the nodes in the list.
   begin
      for J in 0 .. List.Last loop
         Print (List.Items (J), U, Print_Comments, Print_XML_PI, With_URI, Indent => Indent);
      end loop;
   end Print;

end Dom.Core.Nodes.Output;
