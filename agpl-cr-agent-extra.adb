package body Agpl.Cr.Agent.extra is

   ------------------
   -- Remove_Tasks --
   ------------------

   function Remove_Tasks
     (L :    Containers.Lists.List)
      return Containers.Lists.List
   is
      use Containers.Lists;
      R : List;
      procedure Do_It (I : Cursor) is
         A : Agent.Object'Class := Element (I);
      begin
         A.Clear_tasks;
         R.Append (A);
      end Do_It;
   begin
      L.Iterate (Do_It'Access);
      return R;
   end Remove_Tasks;

end Agpl.Cr.Agent.extra;
