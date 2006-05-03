package body Agpl.Htn.Method is

   --------------------
   -- Finished_Child --
   --------------------

   procedure Finished_Child
     (This   : in     Object;
      Parent : in out Tasks.Object'Class;
      Child  : in     Tasks.Object'Class;
      Add    :    out Plan_Node.Node_Access;
      Done   :    out Boolean)
   is
      pragma Unreferenced (This, Parent, Child);
   begin
      Add  := null;
      Done := False;
   end Finished_Child;

   -------------------
   -- Finished_Task --
   -------------------

   procedure Finished_Task
     (This    : in     Object;
      T       : in out Tasks.Object'Class;
      Replace :    out Plan_Node.Node_Access)
   is
      pragma Unreferenced (This, T);
   begin
      Replace := null;
   end Finished_Task;

end Agpl.Htn.Method;
