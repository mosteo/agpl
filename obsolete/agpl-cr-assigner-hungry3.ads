 

with Agpl.Cr.Cost_Matrix;

package Agpl.Cr.Assigner.Hungry3 is

   --  Greedy heuristic that at each step will select the pair agent-task which
   --  adds less cost to the minimax cost.
   --  The new task for an agent will be tried at any point of its plan.

   --  O (T * A * T * T) ~ O (n^4)

   pragma Preelaborate;

   type Object (Keep_Order : Boolean) is new Assigner.Object with null record;
   --  If Keep_Order, any tasks in an Agent passed to Agents will be kept in
   --  that position (at plan start).
   --  If Keep_Order is false, all tasks will be planned in equal conditions.

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return      Assignment.Object;

end Agpl.Cr.Assigner.Hungry3;
