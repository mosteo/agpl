with Agpl.Htn.Method;
with Agpl.Htn.Tasks.Compound;
with Agpl.Htn.Tasks.Primitive;
use Agpl.Htn;

package Htn_Test is

   type Task_Split (Value : Positive) is new
     Tasks.Compound.Object with null record;

   type Task_Divide (Value : Positive) is new
     Tasks.Compound.Object with null record;

   type Task_Consume (Value : Positive) is new
     Tasks.Primitive.Object with null record;

   type Method_Split is new Method.Object with null record;

   function Apply (This : in Method_Split; T : in Tasks.Object'Class)
     return Method.Result;

end Htn_test;
