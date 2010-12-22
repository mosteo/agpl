 

with Agpl.Cr.Cost_Matrix;

package Agpl.Cr.Assigner.Hungry2 is

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List;
      Costs  : in Cr.Cost_Matrix.Object)
      return      Assignment.Object;
   --  Greedy heuristic that will get the agent with the minor cost for its
   --  least costly task.

end Agpl.Cr.Assigner.Hungry2;
