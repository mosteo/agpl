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

--  Plan points are subtrees of actions to be performed inside a Plan.
--  Methods create plan points which will be hooked under the Composite Tasks
--  which have been expanded by the Method.

--  with Agpl.Debug;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Lists;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Agpl.Htn.Plan_Node is

   --  It should ideally be a private package but to avoid a level of indirection
   --  we'll omit that. Clients not using it via Plan can't do anything with it
   --  anyway.

   pragma Elaborate_Body;

   type Object (<>) is limited private;
   --  To prevent creation outside of our control.

   type Node_Access is access Object;
--   for Node_Access'Storage_Pool use Agpl.Debug.Pool;

   type Node_Array is array (Positive range <>) of Node_Access;

   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (Node_Access);
   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node_Access);
   package Task_Id_To_Node is new Ada.Containers.Ordered_Maps
     (Tasks.Task_Id, Node_Access, Tasks."<");

   type Node_Kind is (And_Node, Or_Node, Task_Node);
   subtype Composite_Node_Kind is Node_Kind range And_Node .. Or_Node;

   procedure Append_Child (This, Child : Node_Access);
   --  Append a child node to a node.
   --  If the node was task, its appended to its expansion (must exist).
   --  If the node was AND or OR, the child is appended to the list.

   procedure Build_Index
     (Node  : in     Node_Access;
      Index : in out Task_Id_To_Node.Map;
      Clean : in     Boolean := True);
   --  Recreates the index. If @Clean@, @Index@ is cleared at start.

   function Create (From   : in Tasks.Object'Class;
                    Parent : in Node_Access := null) return Node_Access;
   --  Allocates a node consisting of a single task.

   function Create
     (Kind   : in Composite_Node_Kind;
      From   : in Tasks.Lists.List;
      Parent : in Node_Access := null) return Node_Access;
   --  Allocates a node consisting in several and/or tasks

   function Create
     (Kind   : in Composite_Node_Kind;
      L, R   : in Node_Access;
      Parent : in Node_Access := null) return Node_Access;
   --  Binary tree likewise composition.
   --  If some of these is null, it will not be added.
   --  L.Parent and R.Parent are set to the newly created node.

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Lists.List;
      Parent : in Node_Access := null) return Node_Access;
   --  Multi-leaf tree composition.

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Vectors.Vector;
      Parent : in Node_Access := null) return Node_Access;
   --  Multi-leaf tree composition.

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Array;
      Parent : in Node_Access := null) return Node_Access;
   --  Multi-leaf tree composition.

   function Deep_Copy (This   : in Node_Access;
                       Parent : in Node_Access := null) return Node_Access;
   --  Do a deep copy of this node.
   --  Its parent will be replaced with @Parent@.

   function Deep_Copy_With_Replace
     (This     : in Node_Access;
      Old_Node : in Node_Access;
      New_Node : in Node_Access;
      Parent   : in Node_Access) return Node_Access;
   --  Do a deep copy, but replacing the branch starting at Old_Node with
   --  New_Node.
   --  New_Node.Parent will be the corresponding to the deep copy.

   procedure Delete (This : in out Node_Access);
   --  Frees this allocated node.

   procedure Enumerate_Tasks
     (This           : in     Node_Access;
      Tasks          :    out Htn.Tasks.Lists.List;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False);
   --  Compound/Primitive and Finished/Pending are an AND condition
   --  So you can select Compound AND Finished but not Compound OR Finished

   function Get_Kind (This : in Node_Access) return Node_Kind;

   function Get_Children (This : in Node_Access) return Node_Lists.List;
   --  For And/Or nodes only.

   function Get_Children (This : in Node_Access) return Node_Vectors.Vector;
   --  For And/Or nodes only.

   function Get_Expanded (This : in Node_Access) return Boolean;
   --  For Task nodes only.

   function Get_Expansion (This : in Node_Access) return Node_Access;
   --  Get the expansion child for a task node.

   function Get_Finished (This : Node_Access) return Boolean;
   --  Says if the task of a task node has been finished.
   procedure Set_Finished (This : Node_Access; Finished : Boolean := True);

   function Get_Id (This : not null Node_Access) return String;
   --  The id depends on the tasks
   --  So if the tasks keep the id across plans, the node ids should also
   --  be consistent.

   function Get_Owner (This : in Node_Access) return String;
   procedure Set_Owner (This : in Node_Access; Owner : in String);
   --  Just for task nodes.

   function Get_Task (This : in Node_Access) return Tasks.Object'Class;
   function Get_Task (This : in Node_Access) return Tasks.Object_Access;
   --  For Task nodes only.

   procedure Set_Child (This  : in Node_Access;
                        Child : in Node_Access;
                        Force : in Boolean := False);
   --  For Task nodes, this sets the expansion of the node.

   procedure Set_Children (This     : in Node_Access;
                           Children : in Node_Vectors.Vector;
                           Force    : in Boolean := False);
   --  For AND/OR nodes, this replaces the expansion vector
   --  If not force and the node already has children, Constraint_Error.

   function Get_Parent (This : in Node_Access) return Node_Access;
   --  Get immediate parent node.

   function Is_Ancestor (This    : in Node_Access;
                         Of_This : in Node_Access) return Boolean;
   --  Says if a node is child of another at any distance.

   function Get_Parent_Task (This : in Node_Access) return Node_Access;
   --  Get first ancestor that is a task node or null if none.

   function Get_Random_Sibling (This : in Node_Access) return Node_Access;
   --  For a task node, child of an OR node, will return any of the other
   --  task nodes under the parent OR. If no siblings, of no OR parent,
   --  itself will be returned.

   function Is_Sane (This   : in Node_Access;
                     Parent : in Node_Access := null) return Boolean;
   --  Debug: Check correctness in parent/child pointers.
   --  Call it with Parent => null from the root Node.

private

   type Object (Kind : Node_Kind) is new Ada.Finalization.Limited_Controlled
   with record
      Parent   : Node_Access; -- Will be null for the root.
      Id       : Ustring;     -- Assigned on initialization.

      case Kind is
         when Task_Node =>
            The_Task : Tasks.Object_Access;

            Finished : Boolean := False; --  If the task has been already performed.
            Owner    : Ustring;          --  Name of the agent performing it.

            Child    : Node_Access;
            --  Pointer to the expansion of this node.

         when And_Node | Or_Node =>
            Children : Node_Vectors.Vector;
      end case;
   end record;

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

   procedure Delete_Internal is new Ada.Unchecked_Deallocation
     (Object, Node_Access);

end Agpl.Htn.Plan_Node;
