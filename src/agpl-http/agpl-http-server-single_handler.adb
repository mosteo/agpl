 

--  Version for handlers which return single values (no vectors)

package body Agpl.Http.Server.Single_handler is

   --  Void vector procedure:
   procedure Null_vector (Data : out Sort_handler.Data_set) is
   begin
      Sort_handler.Reset (Data);
   end Null_vector;

end Agpl.Http.Server.Single_handler;
