with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Aws.Messages;
with Aws.Mime;

package body Agpl.Http.Server.Single2_handler is

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data
   is
      use Templates_parser;
   begin
      return
        Aws.Response.Build (
          Aws.Mime.Content_Type (This.Page.all),
          UString'(Parse (
            Get_Root & This.Page.all,
            Standard_Xlats (Request) & This.Single (Request),
            Keep_Unknown_Tags => True,
            Cached            => False)),
          Cache_Control => Aws.Messages.No_Cache);
   end Get_page;

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   --  Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton (Request : in Aws.Status.Data) return Templates_parser.Translate_table is
      pragma Unreferenced (Request);
   begin
      return Templates_parser.No_translation;
   end Void_singleton;

end Agpl.Http.Server.Single2_handler;
