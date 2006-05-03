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
--  $Id: dom-core-nodes-output.ads,v 1.3 2004/01/21 21:05:43 Jano Exp $

with Ada.Strings.Unbounded;

package Dom.Core.Nodes.Output is

   package ASU renames Ada.Strings.Unbounded;

   -- Based on functions from Xml/Ada: 
   -- Doesn't perform encoding.
   procedure Print
     (N              : Node;
      U              : in out ASU.Unbounded_string;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False; 
      Indent         : Natural := 0);
   --  Print the contents of Node and its children in XML format.
   --  If Print_Comments is True, then nodes associated with comments are
   --  also displayed.
   --  The <?xml?> processing instruction is displayed only if Print_XML_PI
   --  By default, names are of the form  ns_prefix:local_name. However, if
   --  with_URI is True, names will be  ns_URI:local_name instead

   procedure Print
     (List           : Dom.Core.Node_List;
      U              : in out ASU.Unbounded_string;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0);
   --  Same as Print, but for all the nodes in the list.

end Dom.Core.Nodes.Output;
