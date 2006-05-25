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

--  The difference with Expres.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Bag;
with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Matrix;
with Agpl.Dynamic_Vector;
with Agpl.Htn.Plan;
with Agpl.Htn.Tasks;
with Agpl.Optimization.Annealing;
with Agpl.Smart_Access;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;

package Agpl.Cr.Mutable_Assignment is

   Log_Section    : constant String := "agpl.cr.mutable_assignment";
   Detail_Section : constant String := "agpl.cr.mutable_assignment.detail";

   Any_Agent    : constant String;
   No_Agent     : constant String;
   Any_Position : constant Positive;

   type Object is tagged private;
   --  No longer is necessary for it to be lightweight, since it's only
   --  copied when a better solution is found, and this doesn't happen
   --  a lot of times. We'll try, however.

   --  Copying should be O(n)

   type Undo_Info is private;

   subtype Cost is Optimization.Annealing.Cost;

   type Mutation_Doer is access procedure (This : in out Object;
                                           Desc :    out Ustring;
                                           Undo :    out Undo_Info);

   type Mutation_Undoer is access procedure (This : in out Object;
                                             Undo : in     Undo_Info);

   ---------------------------
   -- ANNEALING SUBPROGRAMS --
   ---------------------------

   function Evaluate_Minimax (This : in Object) return Cost;
   --  Return Inf if invalid

   function Evaluate_Totalsum (This : in Object) return Cost;
   --  Return Inf if invalid

   procedure Mutate (This : in out Object);
   --  The result can be invalid if restrictions are violated

   function Last_Mutation (This : in Object) return String;
   --  Informative, to know mutations working well
   --  Just returns a description of what was done.

   procedure Undo (This : in out Object);
   --  Must undo the last mutation. Only one level of undo is required.

   ----------------------------
   -- MANAGEMENT SUBPROGRAMS --
   ----------------------------

   procedure Remove_Agent (This : in out Object; Name : in String);
   --  Cut out this agent; its tasks will go to others

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Matrix.Object);
   --  Any task not present here will be assumed is not doable.
   --  Setting the costs, automatically add these agents as alive.

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object);
   --  The tasks are provided in Plan form, inflated or not.

   --------------
   -- MUTATING --
   --------------

   procedure Add_Mutation (This    : in out Object;
                           Mutator : not null Mutation_Doer;
                           Undoer  : not null Mutation_Undoer;
                           Weight  : in     Float   := 1.0);
   --  Probabilities for each mutation are automatically computed from the
   --  weights given here.

   procedure Do_Identity   (This : in out Object;
                            Desc :    out Ustring;
                            Undo :    out Undo_Info);
   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info);
   --  Test mutation, does nothing!

   procedure Do_Flip_Worst   (This : in out Object;
                              Desc :    out Ustring;
                              Undo :    out Undo_Info);
   procedure Undo_Flip_Worst (This : in out Object; Undo : in  Undo_Info);
   --  Attempt to flip a task of the worst agent

   -----------------
   -- CONVERSIONS --
   -----------------

   function To_Assignment   (This : in Object) return Cr.Assignment.Object;
   --  Obtain an assignment from the current configuration.

   procedure Set_Assignment (This : in out Object;
                             Ass  : in     Cr.Assignment.Object);
   --  The assignment given will be used as current solution.
   --  Should check that no pending tasks are unassigned.

private

   --  Each registered mutation to be used
   type Mutation_Handler is record
      Doer   : Mutation_Doer;
      Undoer : Mutation_Undoer;
      Weight : Float;
      Prob   : Optimization.Annealing.Probability;
   end record;

   package Mutation_Vectors is new Dynamic_Vector (Mutation_Handler);

   --  This holds all invariant data accross solutions.
   type Static_Context is record
      Plan      : Htn.Plan.Object;

      Mutations : Mutation_Vectors.Object (First => 1);
   end record;
   type Static_Context_Access is access all Static_Context;

   --  Contains all variable information that is unique to a solution
   type Task_Context is record
      Prev, Next : Htn.Tasks.Task_Id; -- In the agent assignation!!

      Owner         : Ustring;  -- Owner agent
      Owner_Bag_Idx : Positive; -- Index in the per-agent-bag

      General_Bag_Idx : Positive; -- Index in the general bag
   end record;
   type Task_Context_Access is access all Task_Context;

   package Task_Bags is new Bag (Task_Context_Access);
   package Task_Maps is new Ada.Containers.Ordered_Maps
     (Htn.Tasks.Task_Id, Task_Context_Access, Htn.Tasks."<");
   package Task_Bag_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Task_Bags.Object, "<", Task_Bags."=");

   --  Contains all variable information that is unique to a solution
   type Or_Node_Context is record
      Idx_In_Nodes : Positive; -- The index in the general bag of ready OR nodes

      Agent_In_Flip     : Ustring := +No_Agent;  -- Which agent owns this one
      Idx_In_Agent_Flip : Positive;

      Idx_In_Flip   : Positive; -- The index in the undiscrimined flip-ready bag
   end record;
   type Or_Node_Context_Access is access all Or_Node_Context;

   package Or_Node_Bags is new Bag (Or_Node_Context_Access);
   package Or_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Or_Node_Context_Access);
   package Or_Node_Bag_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Or_Node_Bags.Object, "<", Or_Node_Bags."=");

   type Minimax_Key is record
      Cost  : Optimization.Annealing.Cost;
      Agent : Ustring;
   end record;
   function "<" (L, R : Minimax_Key) return Boolean;

   package Agent_Cost_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Optimization.Annealing.Cost, "<", Optimization.Annealing."=");
   package Cost_Agent_Sets is new
     Ada.Containers.Ordered_Sets (Minimax_Key);

   package Smart_Static_Contexts is new
     Smart_Access (Static_Context, Static_Context_Access);

   type Object is new Ada.Finalization.Controlled with record
      --  Invariant information
      Context     : Smart_Static_Contexts.Object;

      --  Currently assigned tasks
      Tasks_By_Id : Task_Maps.Map;

      --  Task per agent:
      Agent_Tasks : Task_Bag_Maps.Map;

      --  All tasks, undiscriminated:
      Tasks_Bag   : Task_Bags.Object (First => 1);

      --  All enabled OR-nodes
      Nodes       : Or_Node_Maps.Map;

      --  OR-nodes with single children that can be instantly flipped, per agent
      Agent_Nodes : Or_Node_Bag_Maps.Map;

      --  All OR nodes with single children for flip, undiscriminated
      Flip_Nodes  : Or_Node_Bags.Object (First => 1);

      --  OR-nodes that are in the assignation, ready to switch a subbranch.
      --  These include all the previous ones, obviously:
      Ready_Nodes : Or_Node_Bags.Object (First => 1);

      --  The current solution costs
      Totalsum    : Optimization.Annealing.Cost;
      Minimax     : Cost_Agent_Sets.Set;
      Agent_Costs : Agent_Cost_Maps.Map;

      --  Undo information
      Last_Mutation_Description : Ustring := +"None";
      Last_Mutation_Index       : Positive;
      Last_Mutation_Undo        : Undo_Info;
   end record;

   type Undo_Info is null record;

   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);

   Any_Agent    : constant String   := "";
   No_Agent     : constant String   := "";
   Any_Position : constant Positive := Positive'Last;

end Agpl.Cr.Mutable_Assignment;
