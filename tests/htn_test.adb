with Text_Io;

package body Htn_Test is

   use Text_Io;

   -----------
   -- Apply --
   -----------

   function Apply
     (This : in Method_Split;
      T : in Tasks.Object'Class)
      return Method.Result
   is
      pragma Unreferenced (This);
      R : Method.Result;
   begin
--      Put ("Applying Split: ");
      if T in Task_Split'Class then
--         Put_Line ("Success");
         Method.Set_Kind (R, Method.And_Decomposition);
         for I in 1 .. Task_Split (T).Value loop
            Method.Add_Task
              (R,
               Task_Consume'(Tasks.Primitive.Object with Value => I));
         end loop;
      elsif T in Task_Divide'Class then
         Method.Set_Kind (R, Method.And_Decomposition);
         Method.Add_Task
           (R, Task_Split'(Tasks.Compound.Object with Value =>
                             Task_Divide (T).Value / 2));
      else
--         Put_Line ("Failure");
         null;
      end if;

      return R;
   end Apply;

end Htn_Test;
