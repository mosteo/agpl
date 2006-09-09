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

with Agpl.Bag; pragma Elaborate_All (Agpl.Bag);
with Agpl.Cr.Agent.Containers;
with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Matrix;
with Agpl.Dynamic_Vector;
with Agpl.Htn.Plan;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Containers;
with Agpl.Optimization.Annealing;
with Agpl.Smart_Access; pragma Elaborate_All (Agpl.Smart_Access);
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;

package Agpl.Cr.Mutable_Assignment is

   pragma Elaborate_Body;

   Log_Section    : constant String := "agpl.cr.mutable_assignment";
   Detail_Section : constant String := "agpl.cr.mutable_assignment.detail";

   type Object is tagged private;
   type Object_Access is access all Object;
   --  No longer is necessary for it to be lightweight, since it's only
   --  copied when a better solution is found, and this doesn't happen
   --  a lot of times. We'll try, however.

   --  Copying should be O(n)

   subtype Agent_Id is String;

   type Undo_Info is private;

   type Mutation_Doer is access procedure (This : in out Object;
                                           Desc :    out Ustring;
                                           Undo :    out Undo_Info);

   type Mutation_Undoer is access procedure (This : in out Object;
                                             Undo : in     Undo_Info);

   ---------------------------
   -- ANNEALING SUBPROGRAMS --
   ---------------------------

   function Evaluate (This      : in Object) return Costs;
   --  Uses the internal criterion

   function Evaluate (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs;

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

   function Normalize (Old_Cost,
                       New_Cost : in Optimization.Cost;
                       Temp     : in Optimization.Annealing.Temperature)
                       return        Optimization.Annealing.Acceptability;

   ----------------------------
   -- MANAGEMENT SUBPROGRAMS --
   ----------------------------

   procedure Add_Agent (This : in out Object; A : in Cr.Agent.Object'class);
   --  No problem to add the same one several times

   procedure Remove_Agent (This : in out Object; Name : in String);
   --  Cut out this agent; its tasks will go to others

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Matrix.Object);
   --  Any task not present here will be assumed is not doable.
   --  *MUST* include costs from the No_Task
   --  *MUST* include cost from No_Task to No_Task (i.e. past incurred costs
   --  of a now idle robot...)

   procedure Set_Criterion (This      : in out Object;
                            Criterion : in     Assignment_Criteria);

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object);
   --  The tasks are provided in Plan form, inflated or not.
   --  Warning! The plan *MUST NOT* contain Starting_Pose tasks.
   --  This is managed internally.
   --  *HOWEVER* the Costs must contemplate the starting task!
   --  No attempt to assignation is made. All dynamic data structures will be
   --  cleared. You should call Create_Some_Solution or To_Assignment
   --  subsequently.

   procedure Create_Some_Solution (This      : in out Object;
                                   Criterion : in Assignment_Criteria);
   --  Prepare the object with a greedy solution.
   --  Basically is a call to To_Assignment with a greedy solution.

   procedure Clear_Dynamic_Part (This : in out Object);
   --  Clear the non-static solution data.
   --  For example as a prepartion step to replace the tasks.
   --  If frees any dynamic memory in use.

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

   procedure Do_Heuristic_1 (This : in out Object;
                             Desc :    out Ustring;
                             Undo :    out Undo_Info);
   procedure Undo_Heuristic_1 (This : in out Object; Undo : in  Undo_Info);
   --  Will consider all agents and tasks to provide some "good" assignment.
   --  The current tasks are re-assigned in a "best pair" greedy fashion.
   --  So no OR node switchings happen.

   procedure Do_Heuristic_2 (This : in out Object;
                             Desc :    out Ustring;
                             Undo :    out Undo_Info);
   procedure Undo_Heuristic_2 (This : in out Object; Undo : in  Undo_Info)
                               renames Undo_Heuristic_1; -- Undo from scratch
   --  This heuristic will consider the best of *all* tasks in every possible
   --  expansion; freeze the plan with the chosen task; repeat until no more T.

   procedure Do_Move_Task (This : in out Object;
                           Desc :    out Ustring;
                           Undo :    out Undo_Info);
   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info);
   --  Will un-move all movements, in the Undo_Info stack, not just one.

--     procedure Do_Move_Task_Changing_Owner (This : in out Object;
--                                            Desc :    out Ustring;
--                                            Undo :    out Undo_Info);
   --  As undo, use the Undo_Move_Task

   -----------------
   -- CONVERSIONS --
   -----------------

   function To_Assignment   (This : in Object) return Cr.Assignment.Object;
   --  Obtain an assignment from the current configuration.

   procedure Set_Assignment (This      : in out Object;
                             Ass       : in     Cr.Assignment.Object;
                             Criterion : in Assignment_Criteria);
   --  The assignment given will be used as current solution.
   --  Any unassigned tasks will be greedily inserted in arbitrary order.
   --  The criterion is used only for previously unassigned tasks.
   --  The dynamic structures will be prepared.

   -----------
   -- DEBUG --
   -----------

   procedure Debug_Dump_Contexts (This : in Object);

private

   package Task_Lists renames Htn.Tasks.Containers.Lists;

   --  Each registered mutation to be used
   type Mutation_Handler is record
      Doer   : Mutation_Doer;
      Undoer : Mutation_Undoer;
      Weight : Float;
      Prob   : Optimization.Annealing.Probability;
   end record;

   package Mutation_Vectors is new Dynamic_Vector (Mutation_Handler);

   package Agent_Maps renames Cr.Agent.Containers.Maps;

   --  This holds all invariant data accross solutions.
   type Static_Context is record
      Agents    : Agent_Maps.Map;

      Costs     : Cr.Cost_Matrix.Object;
      Plan      : Htn.Plan.Object;

      Criterion : Assignment_Criteria := Criterion_Time_Critical;

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
   type Solution_Context_Key is new Ustring;
   type Solution_Context_Ptr is access all Solution_Context'Class;

   function Key (This : in Solution_Context)
                 return Solution_Context_Key is abstract;
   --  Should univocally identify this context in the global map of contexts

   procedure Debug_Dump (This : in Solution_Context) is abstract;
   procedure Common_Dump (This : in Solution_Context'Class);

   package Solution_Context_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Solution_Context_Key,
        Solution_Context'Class);

   type Bag_Context is record
      Key    : Ustring;       -- A bag_key
      Parent : Object_Access; -- Necessary to known the object this bag belongs.
   end record;

   package Solution_Context_Bags is new Agpl.Bag (Solution_Context_Key,
                                                  Bag_Context);

   package Solution_Context_Bag_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Bag_Key, Solution_Context_Bags.Object,
        "<", Solution_Context_Bags."=");

   ------------------
   -- Task_Context --
   ------------------

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
   type Task_Context_Ptr is access all Task_Context'Class;

   function Key (This : in Task_Context) return Solution_Context_Key;
   pragma Inline (Key);

   type Task_Context_Key is new Solution_Context_Key;

   -------------------
   -- Agent_Context --
   -------------------

   type Agent_Context is new Solution_Context with record
      Agent_Name : Ustring;
   end record;

   function Key (This : in Agent_Context) return Solution_Context_Key;
   procedure Debug_Dump (This : in Agent_Context);

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

   ------------------
   -- UNDO SUPPORT --
   ------------------

   procedure Undo_From_Scratch (This : in out Object;
                                Undo : in     Undo_Info);

   --  This record is used to store one moved task so it can be replaced
   --  where it was.
   type Undo_Move_Task_Info is record
      Moved_One  : Htn.Tasks.Task_Id;
      Was_After  : Htn.Tasks.Task_Id := Htn.Tasks.No_Task;
      Was_Before : Htn.Tasks.Task_Id := Htn.Tasks.No_Task;
      Owner_Was  : Ustring;
      --  No id for the last task

      Minsum_Was : Costs;
   end record;

   package Undo_Move_Vectors is
     new Agpl.Dynamic_Vector (Undo_Move_Task_Info, 2);

   type Undo_Info is record
      Ass : Assignment.Object; -- For scratch starting

      Move_Stack : Undo_Move_Vectors.Object (First => 1);
      --  LIFO stack of moved tasks. To undo, just undo movements from tail to H

      Was_Valid  : Boolean;
      --  Says if the previous state was a valid solution.
   end record;

   procedure Reset (This : in out Undo_Info);
   --  Reset the undo information;

   ------------
   -- Object --
   ------------

   type Object is new Ada.Finalization.Controlled with record
      --  Invariant information
      Context     : Smart_Static_Contexts.Object;

      Valid       : Boolean; -- Is the assignment valid?

      Contexts    : Solution_Context_Maps.Map;
      --  All contextual infos (tasks, Or-nodes, etc)
      --  This is the main store where everything can be located.
      --  For now this contains:
      --  All assigned tasks (Task_Context)
      --  All agents (Agent_Context)

      Bags        : Solution_Context_Bag_Maps.Map;
      --  All the different bags
      --  For now this contains:
      --  All assigned tasks (Task_Context.Key)
      --  A bag for each agent, containing its tasks (Task_Context.Key)
      --  A bag with agent contexts (Agent_Context.Key)

      --  The current solution costs
      MinSum      : Costs;
      MinMax      : Cost_Agent_Sets.Set;
      Agent_Costs : Agent_Cost_Maps.Map;

      --  Undo information
      Last_Mutation_Description : Ustring := +"None";
      Last_Mutation_Index       : Positive;
      Last_Mutation_Undo        : Undo_Info;
      Last_Mutation_Exists      : Boolean := False;
   end record;

   --  Controlling...
   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);

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

   --  Tasks
   procedure Adjust_Chain_Removing (This : in out Object;
                                    Job  : in     Task_Context_Ptr);
   procedure Adjust_Chain_Inserting (This         : in out Object;
                                     After_This   : in     Task_Context_Ptr;
                                     Job          : in     Task_Context_Ptr;
                                     Before_This  : in     Task_Context_Ptr);

   function Num_Assigned_Tasks (This : in Object) return Natural;

   procedure Do_Move_Task (This        : in out Object;
                           After_This  : in Task_Context_Ptr;
                           Src         : in Task_Context_Ptr;
                           Before_This : in Task_Context_Ptr;
                           New_Owner   : in Ustring);
   --  Move a task from one point to another
   --  Must maintain all integrity: adjust costs, before/after links, ownership
   --  New owner is necessary if before and after are null

   procedure Do_Remove_Task (This : in out Object;
                             Job  : not null Task_Context_Ptr);
   --  Remove this task from assignation; update all data structures accordingly

   function Get_Task_Context (This : in Object;
                              Id   : in Htn.Tasks.Task_Id)
                              return    Task_Context_Ptr;

   function Ptr (This : in Object;
                 Key  : in Task_Context_Key) return Task_Context_Ptr;
   --  pragma Inline (Ptr);
   --  Gigantic ugly hack probably will blow out everything

   function Ptr (This : in Object;
                 Key  : in Solution_Context_Key) return Solution_Context_Ptr;
   --  pragma Inline (Ptr);

   function Ptr (This : in Object;
                 Key  : in Solution_Context_Key) return Task_Context_Ptr;
   --  pragma Inline (Ptr);

   --  Attributes
   function Get_Attribute (Context : not null access Solution_Context'Class;
                           Attr    : in Solution_Context_Attributes)
                           return String;
   pragma Inline (Get_Attribute);
   function Get_Attribute (Context : in Solution_Context'Class;
                           Attr    : in Solution_Context_Attributes)
                           return String;
   pragma Inline (Get_Attribute);

   procedure Set_Attribute (Context : not null access Solution_Context'Class;
                            Attr    : in     Solution_Context_Attributes;
                            Val     : in     String);
   pragma Inline (Set_Attribute);

   --  Keys
   function Agent_Key (Name : Agent_Id) return Solution_Context_Key;
   pragma Inline (Agent_Key);
   function Task_Key (Id : in Htn.Tasks.Task_Id) return Solution_Context_Key;
   pragma Inline (Task_Key);

   All_Assigned_Tasks : constant Bag_Key := "aat";
   All_Agents         : constant Bag_Key := "aag";

   function Agent_Tasks_Bag (Name : in Agent_Id) return Bag_Key;
   pragma Inline (Agent_Tasks_Bag);
   --  Index for a bag containing some agent all tasks.

   function No_Task_Key return Task_Context_Key; pragma Inline (No_Task_Key);

   --  Bags
   procedure Create_Empty_Bag (This : in out Object;
                               Key  : in     Bag_Key);

   procedure Create_Empty_Bags (This : in out Object);
   --  Create the bags that have to be there, even if empty.

   procedure Add_To_Bag (This    : in out Object;
                         Context : in out Solution_Context'Class;
                         Bag     : in Bag_Key);

   procedure Remove_From_Bag (This    : in out Object;
                              Context : in out Solution_Context'Class;
                              Bag     : in     Bag_Key);

   procedure Remove_From_Bag
     (This    : in out Object;
      Context : in out Solution_Context'Class;
      Bag     : in     Solution_Context_Bag_Maps.Cursor);

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : in     Solution_Context_Ptr);

   procedure Moving_Solution_Context (Context : in out Solution_Context_Key;
                                      Bag     : in out Bag_Context;
                                      Prev,
                                      Curr    : in     Integer);
   pragma Inline (Moving_Solution_Context);

   procedure Reassign_Tasks (This : in out Object; From, To : in Agent_Id);

   --  Costs

   procedure Reevaluate_Agent_Cost (This  : in out Object;
                                    Agent : in     Agent_Id;
                                    Cost  :    out Costs);
   --  This can set This.Valid as false if the agent has invalid costs.
   --  Though, the returned cost will use Cost_For_Invalid_Task and will be
   --  apt for incremental evaluations

   procedure Reevaluate_Minsum (This : in out Object;
                                Cost :    out Costs);
   procedure Reevaluate_Minmax (This : in out Object;
                                Cost :    out Costs);

   --  O (T + log R)
   procedure Reevaluate_Costs (This : in out Object);
   --  Recompute all costs from scratch and update internal cache

   procedure Update_Costs_Inserting
     (This                : in out Object;
      Prev_In_List        : in     Task_Context_Ptr;
      Curr_To_Be_Inserted : in     Task_Context_Ptr;
      Next_In_List        : in     Task_Context_Ptr;
      New_Owner           : in     String);
   --  Update the costs of inserting the Curr task.

   --  O (log R)
   procedure Update_Costs_Removing
     (This               : in out Object;
      Prev_To_Be_Kept    : in     Task_Context_Ptr;
      Curr_To_Be_Deleted : in     Task_Context_Ptr;
      Next_To_Be_Kept    : in     Task_Context_Ptr;
      Former_Owner       : in     String);
   --  Update the costs of removing the Curr task.
   --  Prev or Next can be null

   overriding
   procedure Debug_Dump (This : in Task_Context);

end Agpl.Cr.Mutable_Assignment;
