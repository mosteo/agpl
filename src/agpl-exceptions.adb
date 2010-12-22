package body Agpl.Exceptions is

   use Ada.Exceptions;

   --------------------------
   -- Protected_Occurrence --
   --------------------------

   protected body Protected_Occurrence is

      ---------------------
      -- Check_And_Clear --
      ---------------------

      procedure Check_And_Clear is
      begin
         if Exception_Identity (Occurrence) /= Null_Id then
            declare
               Temp : Exception_Occurrence;
            begin
               Save_Occurrence (Temp, Occurrence);
               Save_Occurrence (Occurrence, Null_Occurrence);
               Reraise_Occurrence (Temp);
            end;
         end if;
      end Check_And_Clear;

      ------------
      -- Is_Set --
      ------------

      function Is_Set return Boolean is
      begin
         return Exception_Identity (Occurrence) /= Null_Id;
      end Is_Set;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Save_Occurrence (Occurrence, Null_Occurrence);
      end Clear;

      -----------
      -- Store --
      -----------

      procedure Store (E : Ada.Exceptions.Exception_Occurrence) is
      begin
         Save_Occurrence (Occurrence, E);
      end Store;

   end Protected_Occurrence;

end Agpl.Exceptions;
