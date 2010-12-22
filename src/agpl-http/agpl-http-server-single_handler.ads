 

--  Version for handlers which return single values (no vectors)

with Agpl.Http.Server.Sort_handler;

package Agpl.Http.Server.Single_handler is

   --  Void vector procedure:
   procedure Null_vector (Data : out Sort_handler.Data_set);
   --  Void singleton
   function Null_singleton return Templates_Parser.Translate_Table renames
      Sort_handler.Void_singleton;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- I.e., must be specified using the Single and Page constraints:
   type Object
      (
      Single : Sort_handler.Singleton_function;
      Page   : Sort_handler.Source_page)
   is new Sort_handler.Object
      (Source => Null_vector'Access,
      Single => Single,
      Page => Page) with null record;

end Agpl.Http.Server.Single_handler;
