 

package body Agpl.Http.Server.Simple_handler is

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data is
   begin
      return This.Handler (Request);
   end Get_page;

end Agpl.Http.Server.Simple_handler;
