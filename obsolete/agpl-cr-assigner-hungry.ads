 

package Agpl.Cr.Assigner.Hungry is

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Lists.List;
      Tasks  : in Agpl.Htn.Tasks.Lists.List)
      return      Assignment.Object;
   --  Greedy heuristic that will get the least used agent and assign to it
   --  the least costly task for him.

end Agpl.Cr.Assigner.Hungry;
