 

--  with Agpl.Cr.Cost_Matrix;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Handle;
with Agpl.Htn.Tasks.Primitive;
with Agpl.Optimization.Annealing;

--  An adhoc assignment with the required operations for annealing
--  and with tasks equivalent one to one

generic
   with function Flip (This : in Htn.Tasks.Primitive.Object'Class)
     return Htn.Tasks.Primitive.Object'Class;
   --  This function should return a primitive task that can replace the one given.
package Agpl.Cr.Assignment.Mutable is

   pragma Elaborate_Body;

   type Object is tagged private;

   function Invalid_Assignment return Object;

   function Mutate (This : in Object) return Object;

   function Normalize (Old_Cost,
                       New_Cost : in Optimization.Annealing.Cost;
                       Temp     : in Optimization.Annealing.Temperature)
                       return        Optimization.Annealing.Acceptability;

private

   type Object is new Agpl.Cr.Assignment.Object with null record;

   procedure Insert_At_Random (This : in out Object;
                               Job  : in     Htn.Tasks.Object'Class);
   --  Insert a given task in a random agent and plan point.

   procedure Select_Random_Task (This : in out Object;
                                 Job  :    out Htn.Tasks.Handle.Object);
   --  Select a random task from any agent and remove it from its list.

end Agpl.Cr.Assignment.Mutable;
