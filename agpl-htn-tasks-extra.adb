package body Agpl.Htn.Tasks.extra is

   use type Htn.Tasks.Task_Id;

   --------------
   -- Contains --
   --------------

   function Contains (T  : Htn.Tasks.Containers.Lists.List;
                      Id : Htn.Tasks.Task_Id)
                      return Boolean
   is
      use Htn.Tasks.Containers.Lists;
      I : Cursor := T.First;
   begin
      while Has_Element (I) loop
         if Element (I).Get_Id = Id then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Contains;

end Agpl.Htn.Tasks.Extra;
