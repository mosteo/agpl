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

   procedure Do_Heuristic_All (This : in out Object;
                               Desc :    out Ustring;
                               Undo :    out Undo_Info);
   procedure Undo_Heuristic_All (This : in out Object; Undo : in  Undo_Info);
   --  Will consider all agents and tasks to provide some "good" assignment.

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
      Costs     : Cr.Cost_Matrix.Object;
      Plan      : Htn.Plan.Object;

      Mutations : Mutation_Vectors.Object (First => 1);
   end record;
   type Static_Context_Access is access all Static_Context;

   type Bag_Index is new String;

   package Index_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Bag_Index, Integer);

   type Solution_Context_Attributes is
     (Owner,
      Is_Flippable);
   --  Attributes for all partial data kept in the solution

   package Attribute_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Solution_Context_Attributes, String);

   type Solution_Context is abstract tagged record
      Attributes  : Attribute_Maps.Map;
      --  A table with all attributes

      Bag_Indexes : Index_Maps.Map;
      --  A table with all the indexes to bags this thing belongs to.
   end record;
   type Solution_Context_Access is access all Solution_Context'Class;
   --  Root for all partial info structures we will need to keep in a solution.

   type Bag_Key is new String;
   type Bag_Context is record
      Key : Ustring; -- A bag_key
   end record;

   package Solution_Context_Bags is new Agpl.Bag (Solution_Context_Access,
                                                  Bag_Context);

   package Solution_Context_Bag_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Bag_Key, Solution_Context_Bags.Object,
        "<", Solution_Context_Bags."=");

   package Solution_Context_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Solution_Context_Access);

   --  Contains all variable information that is unique to a solution
   type Task_Context is new Solution_Context with record
      Job             : Htn.Tasks.Task_Id; -- The task proper.
      Prev, Next      : Htn.Tasks.Task_Id := Htn.Tasks.No_Task;
      --  In the agent assignation.
      --  This could be a pointer for O (1) access instead of O (log n)
      --  But, since all the operations are O (log X), this doesn't makes things
      --  worse, and we avoid deep copying on Object adjust.
      --  The same applies to Job before and to Or_Parent below.
   end record;
   type Task_Context_Access is access all Task_Context;

   --  Contains all variable information that is unique to a solution
   type Or_Node_Context is new Solution_Context with record
      Current_Task      : Htn.Tasks.Task_Id; -- Current selected task
      --  This could be a pointer which would give O (1) instead of O (log T)
      --  access time. We prefer to sachrifice that improvement, since all
      --  mutations are O (log), to avoid deep copy adjusts on Object adjust.
   end record;
   type Or_Node_Context_Access is access all Or_Node_Context;

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

      Valid       : Boolean; -- Is the plan valid?

      Contexts    : Solution_Context_Maps.Map;
      --  All contextual infos (tasks, Or-nodes, etc)
      --  This is the main store where everything can be located.

      Bags        : Solution_Context_Bag_Maps.Map;
      --  All the different bags:
      --  ready tasks, ready tasks per agent, OR-ready, Flip-Ready, Flip per ag.

      --  The current solution costs
      Totalsum    : Optimization.Annealing.Cost;
      Minimax     : Cost_Agent_Sets.Set;
      Agent_Costs : Agent_Cost_Maps.Map;

      --  Undo information
      Last_Mutation_Description : Ustring := +"None";
      Last_Mutation_Index       : Positive;
      Last_Mutation_Undo        : Undo_Info;
   end record;

   type Undo_Kinds is (Identity -- Nothing to undo
                      );

   type Undo_Info is record
      Kind : Undo_Kinds;
   end record;

   --  Controlling...
   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);

   Any_Agent    : constant String   := "";
   No_Agent     : constant String   := "";
   Any_Position : constant Positive := Positive'Last;

   Cost_For_Invalid_Task : constant Cost := 0.0;
   --  We use this as a hack to be able to do cost computations with O (1).
   --  Since we are using incremental evaluation, we can't go to Cost'Last
   --  or else we would lose the known cost of the plan.

   ---------------------
   -- Inner utilities --
   ---------------------

--     procedure Do_Flip_For_Agent (This : in out Object;
--                                  Name : in     String;
--                                  Desc :    out Ustring;
--                                  Undo :    out Undo_Info);
   --  Flip a task of the given agent.

   procedure Do_Temporarily_Remove_Task (This : in out Object;
                                         Job  : not null Task_Context_Access);
   --  Remove this task from assignation; update all data structures accordingly
   --  This assumes that the task will be reinserted, so the OR general bag
   --  won't be touched.

   procedure Moving_Solution_Context (This    : in out Solution_Context_Access;
                                      Context : in out Bag_Context;
                                      Prev,
                                      Curr    : in     Integer);

   procedure Update_Costs_Removing
     (This               : in out Object;
      Prev_To_Be_Kept    : in     Task_Context_Access;
      Curr_To_Be_Deleted : in     Task_Context_Access;
      Next_To_Be_Kept    : in     Task_Context_Access);
   --  Update the costs of removing the Curr task.
   --  Prev or Next can be null

end Agpl.Cr.Mutable_Assignment;
