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

--  The difference with Sancta.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Bag; pragma Elaborate_All (Agpl.Bag);
with Agpl.Cr.Agent.Containers;
with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Cache;
private with Agpl.Cr.Cost_Cache.Handle;
with Agpl.Dynamic_Vector;
with Agpl.Generic_Handle;
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

   type Undo_Info (<>) is private;

   type Mutation_Doer is access procedure (This : in out Object;
                                           Undo :    out Undo_Info);

   type Mutation_Undoer is access procedure (This : in out Object;
                                             Undo : in     Undo_Info);

   ---------------------------
   -- ANNEALING SUBPROGRAMS --
   ---------------------------

   function Evaluate (This      : in Object) return Costs;
   --  Uses the internal criterion
   pragma Inline (Evaluate);

   function Evaluate (This      : in Object) return Optimization.Cost;
   --  For convenience
   pragma Inline (Evaluate);

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

   procedure Clear_Agents (This : in out Object);
   --  Remove all agents

   procedure Remove_Agent (This : in out Object; Name : in String);
   --  Cut out this agent; its tasks will go to others

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Cache.Object'Class);
   --  Any task not present here will be assumed is not doable.
   --  *MUST* include costs from the No_Task
   --  *MUST* include cost from No_Task to No_Task (i.e. past incurred costs
   --  of a now idle robot...)

   procedure Set_Criterion (This      : in out Object;
                            Criterion : in     Assignment_Criteria);

   procedure Set_Tasks (This   : in out Object;
                        Plan   : in     Htn.Plan.Object;
                        Assign : in     Boolean := True);
   --  The tasks are provided in Plan form, inflated or not.
   --  Warning! The plan *MUST NOT* contain Starting_Pose tasks.
   --  This is managed internally.
   --  *HOWEVER* the Costs must contemplate the starting task!
   --  All dynamic data structures will be
   --  cleared. You should call Create_Some_Solution or To_Assignment
   --  subsequently, unless Assign is true that causes an initial greedy
   --  assignment, trying to keep the previous assignment.

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
                            Undo :    out Undo_Info);
   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info);
   --  Test mutation, does nothing!

   procedure Do_Heuristic_1 (This : in out Object;
                             Undo :    out Undo_Info);
   --  Will consider all agents and tasks to provide some "good" assignment.
   --  The current tasks are re-assigned in a "best pair" greedy fashion.
   --  So no OR node switchings happen.

   procedure Undo_From_Scratch (This : in out Object; Undo : in Undo_Info);
   --  Undo for heuristics

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
   --  Any tasks in Ass but not in This.Plan will be discarded.
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

      Costs     : Cost_Cache.Handle.Object;
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

   ----------------
   -- OR_Context --
   ----------------
   type Or_Context is new Solution_Context with record
      Node   : Htn.Plan.Subplan;
      Branch : Htn.Plan.Subplan;
   end record;
   --  Since the plan is kept unmodified, there is no problem using accesses
   --  here.

   function Or_Key (This : in String) return Solution_Context_Key;
   pragma Inline (Or_Key);
   --  Get a key form a node ID
   function Key (This : in Or_Context) return Solution_Context_Key;
   pragma Inline (Key);
   procedure Debug_Dump (This : in Or_Context);

   type Minimax_Key is record
      Cost  : Costs;
      Agent : Ustring;
   end record;
   function "<" (L, R : Minimax_Key) return Boolean;

   package Agent_Cost_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Agent_Id, Costs, "<", Cr."=");
   package Cost_Agent_Sets is new
     Ada.Containers.Ordered_Sets (Minimax_Key);

   package Smart_Static_Contexts is new
     Smart_Access (Static_Context, Static_Context_Access);

   ------------------
   -- UNDO SUPPORT --
   ------------------

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

   type Undo_Kinds is (Identity, From_Scratch, Move_Task, Switch_Or_Node);

   type Undo_Internal (Kind : Undo_Kinds) is record
      Description : Ustring;

      case Kind is
         when Identity =>
            null;
         when From_Scratch =>
            Ass : Assignment.Object := Assignment.Empty_Object;
            --  For scratch starting
         when Move_Task =>
            Move_Stack : Undo_Move_Vectors.Object (First => 1);
            --  LIFO stack of moved tasks. To undo, just undo movements from
            --  tail to head
         when Switch_Or_Node =>
            Actived_Or_Branch : Htn.Plan.Subplan := null;
            --  We will de-activate this branch when undoing
            Or_Stack          : Undo_Move_Vectors.Object (First => 1);
            --  The tasks that were removed must be replaced where they were,
            --  activating their respective OR nodes...
      end case;
   end record;

   package Undo_Handle is new Agpl.Generic_Handle (Undo_Internal);

   type Undo_Info is record
      Handle : Undo_Handle.Object;
   end record;

   procedure Reset (This : in out Undo_Info);
   --  Reset the undo information;

   ------------
   -- Object --
   ------------

   type Object is new Ada.Finalization.Controlled with record
      --  Invariant information
      Context     : Smart_Static_Contexts.Object;

      Valid       : Boolean := True;
      --  Is the assignment valid? Initially empty (hence valid).

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
      --  A bag with active OR nodes (Or_Context_Key)

      --  The current solution costs
      MinSum      : Costs;
      MinMax      : Cost_Agent_Sets.Set;
      Agent_Costs : Agent_Cost_Maps.Map;

      --  Undo information
      Was_Valid          : Boolean            := False;
      --  Previous object state.

      Last_Mutation      : Ustring; -- Cached copy

      Undo               : Undo_Info := (Handle => Undo_Handle.Null_Object);
      Undoer             : Mutation_Undoer;
      --  Info needed to reconstruct object.
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

   --  TASKS --
   No_Task : Htn.Tasks.Task_Id renames Htn.Tasks.No_Task;

   procedure Adjust_Chain_Removing (This : in out Object;
                                    Job  : in     Task_Context_Ptr);
   procedure Adjust_Chain_Inserting (This         : in out Object;
                                     After_This   : in     Task_Context_Ptr;
                                     Job          : in     Task_Context_Ptr;
                                     Before_This  : in     Task_Context_Ptr);

   function Num_Assigned_Tasks (This : in Object) return Natural;

   procedure Add_Undo_Move (This : in     Object;
                            Job  : in     Task_Context_Ptr;
                            Undo : in out Undo_Internal);

   procedure Do_Insert_Task (This        : in out Object;
                             After_This  : in     Task_Context_Ptr;
                             Src         : in     Task_Context'Class;
                             Before_This : in     Task_Context_Ptr;
                             New_Owner   : in     Agent_Id);
   --  Src is not a pointer because the task is no longer in the list of
   --  contexts!
   --  The bag indexes are automatically cleared/created, so no need to do this
   --  in advance

   procedure Do_Move_Task (This        : in out Object;
                           After_This  : in     Task_Context_Ptr;
                           Src         : in out Task_Context_Ptr;
                           Before_This : in     Task_Context_Ptr;
                           New_Owner   : in     Agent_Id);
   --  Warning: Src will change inside this call

   procedure Do_Remove_Task (This : in out Object;
                             Job  : in out  Task_Context_Ptr);
   --  Remove this task from assignation; update all data structures accordingly
   --  Job will be null after removal, since there's no longer a context for it.
   --  Better make a copy before if you need it.

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

   -- ATTRIBUTES --
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

   -- KEYS --
   function Agent_Key (Name : Agent_Id) return Solution_Context_Key;
   pragma Inline (Agent_Key);
   function Task_Key (Id : in Htn.Tasks.Task_Id) return Solution_Context_Key;
   pragma Inline (Task_Key);

   All_Assigned_Tasks  : constant Bag_Key := "aat";
   All_Agents          : constant Bag_Key := "aag";
   All_Active_Or_Nodes : constant Bag_Key := "aaon";

   function Agent_Tasks_Bag (Name : in Agent_Id) return Bag_Key;
   pragma Inline (Agent_Tasks_Bag);
   --  Index for a bag containing some agent all tasks.

   function No_Task_Key return Task_Context_Key; pragma Inline (No_Task_Key);

   -- BAGS --
   function Bag_Length (This : in Object; Key : in Bag_Key) return Natural;

   procedure Create_Empty_Bag (This : in out Object;
                               Key  : in     Bag_Key);

   procedure Create_Empty_Bags (This : in out Object);
   --  Create the bags that have to be there, even if empty.

   procedure Add_To_Bag (This    : in out Object;
                         Context : in out Solution_Context'Class;
                         Bag     : in Bag_Key);

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : access Solution_Context'Class);

   procedure Remove_From_Bag (This    : in out Object;
                              Context : in out Solution_Context'Class;
                              Bag     : in     Bag_Key);

   procedure Remove_From_Bag
     (This    : in out Object;
      Context : in out Solution_Context'Class;
      Bag     : in     Solution_Context_Bag_Maps.Cursor);

   procedure Moving_Solution_Context (Context : in out Solution_Context_Key;
                                      Bag     : in out Bag_Context;
                                      Prev,
                                      Curr    : in     Integer);
   pragma Inline (Moving_Solution_Context);

   procedure Reassign_Tasks (This : in out Object; From, To : in Agent_Id);

   procedure Select_Random_Insertion (This  : in     Object;
                                      Bag   : in     Bag_Key;
                                      Prev  :    out Task_Context_Ptr;
                                      Curr  :    out Task_Context_Ptr;
                                      Next  :    out Task_Context_Ptr);
   --  Select a random insertion point in a task bag
   --  Prev, curr and next can be  both null
   --  Curr will be equal to prev or next, since we insert before or after
   --  a random task
   --  If sthe bag was empty, the three will be null.

   function Select_Random_Context (This : in     Object;
                                   Bag  : in     Bag_Key) return Solution_Context_Ptr;

   function Select_Random_Task (This : in     Object;
                                Bag  : in     Bag_Key) return Task_Context_Ptr;
   --  Select a random task in a task bag (CURR)

   -- COSTS --

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

   function Evaluate_Cost_Inserting
     (This                : in Object;
      Prev_In_List        : in Task_Context_Ptr;
      Curr_To_Be_Inserted : in Htn.Tasks.Task_Id;
      Next_In_List        : in Task_Context_Ptr;
      New_Owner           : in Agent_Id) return Cr.Costs;
   --  Gives the criterion evaluated result of inserting a task in a point.
   --  No changes are made.
   --  This can return Infinite, unlike the modificators who keep "deltable" costs.

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

   -- CONTEXTS --
   procedure Remove_Context (This : in out Object;
                             Key  : in Solution_Context_Key);
   --  Remove from contexts list and bags

   procedure Add_Or_Contexts (This : in out Object;
                              Node : in     Htn.Plan.Subplan);
   --  Will check if Id parent is an OR context and if so add it.
   --  In any case it will climb up until root is reached.

   procedure Descend_Removing (This : in out Object;
                               Node : in     Htn.Plan.Subplan;
                               Undo : in out Undo_Internal);

   --  FUNCTIONS THAT MANIPULATE BAGS AND HAVE TO BE UPDATED ON ADDITION OF
   --  NEW BAGS:
   --  Set_Assignment
   --  Do_Insert_Task
   --  Descend_Removing
   --  Descend_Adding
   --  Add_Or_Nodes

end Agpl.Cr.Mutable_Assignment;
