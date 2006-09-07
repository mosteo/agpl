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
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;

package Agpl.Cr.Mutable_Assignment is

   --  pragma Elaborate_Body;

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

   procedure Add_Agent (This : in out Object; Name : in Agent_Id);
   --  No problem to add the same one several times

   procedure Remove_Agent (This : in out Object; Name : in Agent_Id);
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

   package Agent_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Agent_Id);

   --  This holds all invariant data accross solutions.
   type Static_Context is record
      Agents    : Agent_Sets.Set;

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
   type Solution_Context_Pointer is access all Solution_Context'Class;
   --  Root for all partial info structures we will need to keep in a solution.

   procedure Debug_Dump (This : in Solution_Context) is abstract;

   package Context_Pointers is new Smart_Access (Solution_Context'Class,
                                                 Solution_Context_Pointer);
   subtype Solution_Context_Access is Context_Pointers.Object;

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
     (Solution_Context_Key, Solution_Context_Access, "<", Context_Pointers."=");

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

   ------------------
   -- UNDO SUPPORT --
   ------------------

   procedure Clear_Undo (This : in out Object);

   procedure Undo_From_Scratch (This : in out Object;
                                Undo : in     Undo_Info);

   --  This record is used to store one moved task so it can be replaced
   --  where it was.
   type Undo_Move_Task_Info is record
      Moved_One  : Htn.Tasks.Task_Id;
      Was_Before : Htn.Tasks.Task_Id := Htn.Tasks.No_Task;
      --  No id for the last task
   end record;

   package Undo_Move_Vectors is
     new Agpl.Dynamic_Vector (Undo_Move_Task_Info, 2);

   type Undo_Info is record
      Ass : Assignment.Object; -- For scratch starting

      Move_Stack : Undo_Move_Vectors.Object (First => 1);
      --  LIFO stack of moved tasks. To undo, just undo movements from tail to H
   end record;

   ------------
   -- Object --
   ------------

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

   procedure Debug_Dump_Contexts (This : in Object);

   function Is_Sane (This : in Object) return Boolean;
   --  Check for data structures sanity
   --  Can be expensive, use it only for debugging.

   --  Tasks
   function Num_Assigned_Tasks (This : in Object) return Natural;

   procedure Do_Move_Task (This       : in out Object;
                           Src        : in Task_Context_Access;
                           Bfr        : in Task_Context_Access;
                           New_Owner  : in Ustring);
   --  Move a task from one point to another
   --  Must maintain all integrity: adjust costs, before/after links, ownership
   --  New owner is necessary if Bfr is No_Task!

   procedure Do_Remove_Task (This : in out Object;
                             Job  : not null Task_Context_Access);
   --  Remove this task from assignation; update all data structures accordingly

   function Get_Task_Context (This : in Object;
                              Id   : in Htn.Tasks.Task_Id)
                              return    Task_Context_Access;

   --  Attributes
   function Get_Attribute (This : in Solution_Context_Pointer;
                           Attr : in Solution_Context_Attributes) return String;

   procedure Set_Attribute (This : in Solution_Context_Pointer;
                            Attr : in Solution_Context_Attributes;
                            Val  : in String);

   --  Keys
   function Task_Key (Id : in Htn.Tasks.Task_Id) return Solution_Context_Key;
   pragma Inline (Task_Key);

   All_Assigned_Tasks : constant Bag_Key := "aat";

   --  Bags
   procedure Create_Empty_Bags (This : in out Object);
   --  Create the bags that have to be there, even if empty.

   procedure Add_To_Bag (This    : in out Object;
                         Context : in Solution_Context_Access;
                         Bag     : in Bag_Key);

   procedure Remove_From_Bag (This    : in out Object;
                              Context : in     Solution_Context_Pointer;
                              Bag     : in     Bag_Key);

   procedure Remove_From_Bag
     (This    : in out Object;
      Context : in     Solution_Context_Pointer;
      Bag     : in     Solution_Context_Bag_Maps.Cursor);

   procedure Remove_From_All_Bags (This    : in out Object;
                                   Context : in     Solution_Context_Pointer);

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

   --  O (T + log R)
   procedure Reevaluate_Costs (This : in out Object);
   --  Recompute all costs from scratch and update internal cache

   procedure Update_Costs_Inserting
     (This                : in out Object;
      Prev_In_List        : in     Task_Context_Access;
      Curr_To_Be_Inserted : in     Task_Context_Access;
      Next_In_List        : in     Task_Context_Access);
   --  Update the costs of inserting the Curr task.

   --  O (log R)
   procedure Update_Costs_Removing
     (This               : in out Object;
      Prev_To_Be_Kept    : in     Task_Context_Access;
      Curr_To_Be_Deleted : in     Task_Context_Access;
      Next_To_Be_Kept    : in     Task_Context_Access);
   --  Update the costs of removing the Curr task.
   --  Prev or Next can be null

   overriding
   procedure Debug_Dump (This : in Task_Context);

   overriding
   procedure Debug_Dump (This : in Or_Node_Context);

end Agpl.Cr.Mutable_Assignment;
