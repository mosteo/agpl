 

with Aws.Response;
with Aws.Status;

package Agpl.Http.Server.Simple_handler is

   ------------------------------------------------------------------------
   -- Handler                                                            --
   ------------------------------------------------------------------------
   type Handler_function is access 
      function (Request : in Aws.Status.Data) return Aws.Response.Data;

   type Object (Handler : Handler_function) is new 
      Handler_object with null record;

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data;

end Agpl.Http.Server.Simple_handler;
