

with Aws.Response;
with Aws.Status;
with Templates_Parser;

package Agpl.Http.Server is

   Log_Section : constant String := "agpl.http.server";

   ------------------------------------------------------------------------
   -- Handlers                                                           --
   ------------------------------------------------------------------------
   type Handler_Function is access
     function (Request : in Aws.Status.Data) return Aws.Response.Data;

   type Handler_Object is abstract tagged null record;

   --  This function is the one called to obtain the data, and thus the one
   --  you must overload in Handler_object descendant types to implement new
   --  behavior.
   --  This default implementation will simply call Handler.Handler
   function Get_Page (
                      Handler : in Handler_Object;
                      Request : in Aws.Status.Data) return Aws.Response.Data is abstract;

   ------------------------------------------------------------------------
   -- Callback_function                                                  --
   ------------------------------------------------------------------------
   --  Default callback function which dispatches to the proper handler
   --  Also acts as a server of static pages.
   --  Also dispatches to the SOAP handler (below) if registered.
   function Callback_Function (Request : in AWS.Status.Data)
                               return AWS.Response.Data;

   ------------------------------------------------------------------------
   -- Register_user_pass                                                 --
   ------------------------------------------------------------------------
   --  Stores an user/pass; currently only a pair is stored.
   --  The server must have started with session support.
   procedure Register_User_Pass (User : in String; Pass : in String);

   ------------------------------------------------------------------------
   -- Register_soap                                                      --
   ------------------------------------------------------------------------
   --  Basic SOAP management: only a custom handler can be registered:
   procedure Register_Soap (Handler : in Handler_Function);

   ------------------------------------------------------------------------
   -- Register handler                                                   --
   ------------------------------------------------------------------------
   --  Register some handler.
   procedure Register_Handler (
                               URI : in String; Handler : in Handler_Object'Class);

   ------------------------------------------------------------------------
   -- Request_Redirect                                                   --
   ------------------------------------------------------------------------
   --  "To" should be the string for a registered handler
   function Request_Redirect (
                              From : in Aws.Status.Data; To : in String) return Aws.Response.Data;

   ------------------------------------------------------------------------
   -- Get_root                                                           --
   ------------------------------------------------------------------------
   function Get_Root return String;

   ------------------------------------------------------------------------
   -- Set_root                                                           --
   ------------------------------------------------------------------------
   --  Says what's the root folder to prepend to all requests
   procedure Set_Root (Root : in String := "");

   ----------------------------
   -- Set_Embedded_Operation --
   ----------------------------
   procedure Enable_Embedded (Active : Boolean := True);
   --  Enable the use of embedded resources...

   ----------------------
   -- Embedded_Enabled --
   ----------------------
   function Embedded_Enabled return Boolean;

   ------------------------------------------------------------------------
   -- Get_server_name                                                    --
   ------------------------------------------------------------------------
   function Get_Server_Name return String;

   ------------------------------------------------------------------------
   -- Set_server_name                                                    --
   ------------------------------------------------------------------------
   --  String used for the @_VERSION_@ tag
   procedure Set_Server_Name (Version : in String := "");

   ------------------------------------------------------------------------
   -- Set_style_sheet                                                    --
   ------------------------------------------------------------------------
   --  Sets the file to be used as style
   procedure Set_Style_Sheet (Sheet : in String := "");

   ------------------------------------------------------------------------
   -- Get_style_sheet                                                    --
   ------------------------------------------------------------------------
   function Get_Style_Sheet return String;

   ------------------------------------------------------------------------
   --  Standard_xlats                                                     --
   ------------------------------------------------------------------------
   --  Returns a translation table for the default Agpl tags:
   --  VERSION <-- Get_server_name
   --  STYLE   <-- Get_style_sheet
   function Standard_Xlats (Request : in Aws.Status.Data)
                            return Templates_Parser.Translate_Table;
   pragma Inline (Standard_Xlats);

end Agpl.Http.Server;
