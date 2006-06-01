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
with Agpl.Generic_Handle;
with Agpl.Htn.Plan;
with Agpl.Htn.Tasks;
with Agpl.Optimization.Annealing;
with Agpl.Smart_Access;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;

package Agpl.Cr.Mutable_Assignment is

   Log_Section    : constant String := "agpl.cr.mutable_assignment";
   Detail_Section : constant String := "agpl.cr.mutable_assignment.detail";

   type Agent_Id is new String;

   Any_Agent    : constant Agent_Id;
   No_Agent     : constant Agent_Id;
   Any_Position : constant Positive;

   type Object is tagged private;
   --  No longer is necessary for it to be lightweight, since it's only
   --  copied when a better solution is found, and this doesn't happen
   --  a lot of times. We'll try, however.

   --  Copying should be O(n)

   type Undo_Info is private;

   type Mutation_Doer is access procedure (This : in out Object;
                                           Desc :    out Ustring;
                                           Undo :    out Undo_Info);

   type Mutation_Undoer is access procedure (This : in out Object;
                                             Undo : in     Undo_Info);

   ---------------------------
   -- ANNEALING SUBPROGRAMS --
   ---------------------------

   function Evaluate_Minimax (This : in Object) return Costs;
   --  Return Inf if invalid

   function Evaluate_Totalsum (This : in Object) return Costs;
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

   procedure Add_Agent (This : in out Object; Name : in Agent_Id);

   procedure Remove_Agent (This : in out Object; Name : in Agent_Id);
   --  Cut out this agent; its tasks will go to others

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Matrix.Object);
   --  Any task not present here will be assumed is not doable.
   --  *MUST* include costs from the No_Task
   --  *MUST* include cost from No_Task to No_Task (i.e. past incurred costs
   --  of a now idle robot...)

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object);
   --  The tasks are provided in Plan form, inflated or not.
   --  Warning! The plan *MUST NOT* contain Starting_Pose tasks.
   --  This is managed internally.
   --  *HOWEVER* the Costs must contemplate the starting task!

   procedure Clear_Dynamic_Part (This : in out Object);
   --  Clear the non-static solution data.
   --  For example as a prepartion step to replace the tasks.

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

   package Agent_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Agent_Id);

   --  This holds all invariant data accross solutions.
   type Static_Context is record
      Agents    : Agent_Sets.Set;

      Costs     : Cr.Cost_Matrix.Object;
      Plan      : Htn.Plan.Object;

      Mutations : Mutation_Vectors.Object (First => 1);
   end record;
   type Static_Context_Access is access all Static_Context;

   type Bag_Key is new String;

   package Index_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Bag_Key, Integer);

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
      --  A table with all the indexes in bags this thing belongs to.
   end record;
   type Solution_Context_Access is access all Solution_Context'Class;
   --  Root for all partial info structures we will need to keep in a solution.

   type Bag_Context is record
      Key : Ustring; -- A bag_key
   end record;

   package Solution_Context_Bags is new Agpl.Bag (Solution_Context_Access,
                                                  Bag_Context);

   package Solution_Context_Bag_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Bag_Key, Solution_Context_Bags.Object,
        "<", Solution_Context_Bags."=");

   type Solution_Context_Key is new String;

   package Solution_Context_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Solution_Context_Key, Solution_Context_Access);

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
      Cost  : Costs;
      Agent : Ustring;
   end record;
   function "<" (L, R : Minimax_Key) return Boolean;

   package Agent_Cost_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Agent_Id, Costs, "<", Optimization."=");
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
      --  For now this contains:
      --  All assigned tasks

      Bags        : Solution_Context_Bag_Maps.Map;
      --  All the different bags
      --  For now this contains:
      --  All assigned tasks

      --  The current solution costs
      Totalsum    : Costs;
      Minimax     : Cost_Agent_Sets.Set;
      Agent_Costs : Agent_Cost_Maps.Map;

      --  Undo information
      Last_Mutation_Description : Ustring := +"None";
      Last_Mutation_Index       : Positive;
      Last_Mutation_Undo        : Undo_Info;
   end record;

   --  Controlling...
   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);

   Any_Agent    : constant Agent_Id := "";
   No_Agent     : constant Agent_Id := "";
   Any_Position : constant Positive := Positive'Last;

   Cost_For_Invalid_Task : constant Costs := 0.0;
   --  We use this as a hack to be able to do cost computations with O (1).
   --  Since we are using incremental evaluation, we can't go to Cost'Last
   --  or else we would lose the known cost of the plan.

   ---------------------
   -- Inner utilities --
   ---------------------

   function Is_Sane (This : in Object) return Boolean;
   --  Check for data structures sanity
   --  Can be expensive, use it only for debugging.

   --  Attributes
   function Get_Attribute (This : in Solution_Context_Access;
                           Attr : in Solution_Context_Attributes) return String;

   procedure Set_Attribute (This : in Solution_Context_Access;
                            Attr : in Solution_Context_Attributes;
                            Val  : in String);

   --  Keys
   function Task_Key (Id : in Htn.Tasks.Task_Id) return Solution_Context_Key;
   pragma Inline (Task_Key);

   --  Bags
   procedure Add_To_Bag (This    : in out Object;
                         Context : in Solution_Context_Access;
                         Bag     : in Bag_Key);

   procedure Remove_From_Bag (This    : in out Object;
                              Context : in     Solution_Context_Access;
                              Bag     : in     Bag_Key);

   procedure Remove_From_Bag
     (This    : in out Object;
      Context : in     Solution_Context_Access;
      Bag     : in     Solution_Context_Bag_Maps.Cursor);

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : in     Solution_Context_Access);

   procedure Do_Remove_Task (This : in out Object;
                             Job  : not null Task_Context_Access);
   --  Remove this task from assignation; update all data structures accordingly

   procedure Moving_Solution_Context (Context : in out Solution_Context_Access;
                                      Bag     : in out Bag_Context;
                                      Prev,
                                      Curr    : in     Integer);
   pragma Inline (Moving_Solution_Context);

   procedure Reassign_Tasks (This : in out Object; From, To : in Agent_Id);

   --  Costs

   function Reevaluate_Agent_Cost (This : in Object; Agent : in Agent_Id)
                                   return    Costs;
   function Reevaluate_Totalsum   (This : in Object)
                                   return    Costs;
   function Reevaluate_Minimax    (This : in Object)
                                   return    Costs;

   procedure Reevaluate_Costs (This : in out Object);
   --  Recompute all costs from scratch and update internal cache

   procedure Update_Costs_Removing
     (This               : in out Object;
      Prev_To_Be_Kept    : in     Task_Context_Access;
      Curr_To_Be_Deleted : in     Task_Context_Access;
      Next_To_Be_Kept    : in     Task_Context_Access);
   --  Update the costs of removing the Curr task.
   --  Prev or Next can be null

   ------------------
   -- UNDO SUPPORT --
   ------------------

   package Object_Handlers is new Agpl.Generic_Handle
     (Ada.Finalization.Controlled'Class);
   --  Must be so, because Object is incomplete at this point.
   --  A simple type conversion should do the trick, even if a check is incurred

   procedure Undo_From_Scratch (This : in out Object;
                                Undo : in     Undo_Info);

   type Undo_Info is record
      Scratch : Object_Handlers.Object; -- For heavy mutations
   end record;

end Agpl.Cr.Mutable_Assignment;
