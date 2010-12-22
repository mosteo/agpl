package body Agpl.Containers.Ordered_Multidimensional_Maps is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Key_Array) return Boolean is
   begin
      for I in L'Range loop
         if L (I) < R (I) then
            return True;
         elsif not (L (I) = R (I)) then
            return False;
         end if;
      end loop;

      return False;
   end "<";

end Agpl.Containers.Ordered_Multidimensional_Maps;
