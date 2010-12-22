

with Aws.Response;
with Aws.Status;
with Templates_parser;

--  Like the single handler, but the Translation function receives the request as parameter
--  so it can act depending on it.

package Agpl.Http.Server.Single2_handler is

   --  And also with functions that return singletons (values not in tables):
   type Singleton_function is access
      function (Request : in Aws.Status.Data) return Templates_parser.Translate_table;

   --  The source page is simply an access to a string:
   type Source_page is access String;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object (
      Single : Singleton_function;
      Page   : Source_page)
   is new Handler_object with null record;

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data;

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   --  Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton (Request : in Aws.Status.Data) return Templates_parser.Translate_table;
   pragma Inline (Void_singleton);
   function Null_singleton (Request : in Aws.Status.Data) return Templates_parser.Translate_table
      renames Void_singleton;

end Agpl.Http.Server.Single2_handler;
