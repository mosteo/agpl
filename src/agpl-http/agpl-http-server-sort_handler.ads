

with AWS.Response;
with AWS.Status;
with Templates_Parser;

with Agpl.Containers.Naked_Vectors;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Agpl.Http.Server.Sort_handler is

   Default_Lines_Per_Page : constant := 1000;

   ---------------
   -- Data_cell --
   ---------------
   --  Each cell will be passed in this record:
   type Data_cell is record
      Value       : UString; -- Data to be displayed
      Order_value : UString; -- Value to sort with
   end record;

   --  Each row is an array of Data_entries:
   package Datarows is new
      Agpl.Containers.Naked_Vectors (Data_cell, Initial_size => 20);

   --------------
   -- Data_row --
   --------------
   type Data_row is new Datarows.Object (First => 1) with null record;

   --  The entire dataset is a dynamic vector of Data_rows:
   package Datasets is new
      Agpl.Containers.Naked_Vectors (
         Data_row, Initial_size => 100, Grow_factor => 2.0);

   --------------
   -- Data_set --
   --------------
   type Data_set is new Datasets.Object (First => 1) with null record;

   --  The Sort_Handler will work with functions that return datasets:
   type Source_procedure is access procedure (Data : out Data_set);

   --  And also with functions that return singletons (values not in tables):
   type Singleton_function is access
      function return Templates_Parser.Translate_Table;

   --  The source page is simply an access to a string:
   type Source_page is access String;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object (
      Source : Source_procedure;
      Single : Singleton_function;
      Page   : Source_page)
   is new Handler_Object with private;

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   --  There are several arguments that can be specified in the HTTP request:
   --  orden=positive    : column to order the listing
   --  sentido=boolean   : true=descending, false=ascending
   --  page=positive     : which page of the listing to retrieve
   --  lines=positive    : num of lines in each page
   --  A special tag PAGER will be replaced with links for the pages.
   --  It has class="pager".
   function Get_page (
      This    : in Object;
      Request : in AWS.Status.Data) return AWS.Response.Data;

   ------------------------------------------------------------------------
   -- Set_Lines_Per_Page                                                 --
   ------------------------------------------------------------------------
   --  To change the default number of records per page.
   procedure Set_Lines_Per_Page (This : in out Object; Lines : in Positive);

   ------------------------------------------------------------------------
   -- Set_settings_file                                                  --
   ------------------------------------------------------------------------
   --  To indicate where (path + name) to save ordering prefs.
   procedure Set_settings_file (This : in String);

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   --  Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton return Templates_Parser.Translate_Table;
   pragma Inline (Void_singleton);
   function Null_singleton return Templates_Parser.Translate_Table
      renames Void_singleton;

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   --  Auxiliary to ease creation of sorting fields based in integers
   function Rpad (I : in Integer;  Size : in Natural := 11) return UString;
   function Rpad (I : in Float;    Size : in Natural := 11) return UString;
   function Rpad (I : in Duration; Size : in Natural := 11) return UString;

private

   type Object (
      Source : Source_procedure;
      Single : Singleton_function;
      Page   : Source_page)
   is new Handler_Object with record
      Lines_Per_Page : Positive := Default_Lines_Per_Page;
   end record;

end Agpl.Http.Server.Sort_handler;
