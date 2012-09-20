with Ada.Containers.Indefinite_Ordered_Maps;

with Agpl.Calendar.Format;
with Agpl.Debug;
with Agpl.Exceptions;
with Agpl.Strings;        use Agpl.Strings;
with Agpl.Strings.Fields; use Agpl.Strings.Fields;
with Agpl.Trace;          use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Aws.Messages;
with Aws.Mime;
with Aws.Parameters;
with Aws.Resources.Embedded;
with AWS.Status.Set;
use  AWS;

with Gnat.Os_Lib;

with Text_Io;

package body Agpl.Http.Server is

   Embedded : Boolean := False;

   --  Root:
   Root : Ustring;

   --  Server name:
   Server_Name : Ustring;
   --  Style sheet:
   Style_Sheet : Ustring;

   --  Auth:
   Username : Ustring;
   Password : Ustring;

   --  Handlers:
   type Handler_Access is access Handler_Object'Class;
   package Disp_Tables is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Handler_Access);
   Dispatchers : Disp_Tables.Map;

   --  Ahead declaration
   function Dispatcher_404 (Request : in Aws.Status.Data)
                            return Aws.Response.Data;

   --  SOAP handler
   Soap_Handler : Handler_Function;

   ---------------
   -- Authorize --
   ---------------
   function Authorize (Request : in AWS.Status.Data)
                       return Aws.Response.Data
   is
   begin
      return Aws.Response.Authenticate (
                                        "You are coming from " & Aws.Status.Peername (Request) &
                                        ". Your papers, please:", Aws.Response.Basic);
   end Authorize;

   ----------------
   -- Check_user --
   ----------------
   function Check_User (Request : in AWS.Status.Data) return Boolean is
      User  : constant String := Aws.Status.Authorization_Name (Request);
      Pwd   : constant String := Aws.Status.Authorization_Password (Request);
      Addr  : constant String := Aws.Status.Peername (Request);
   begin
      if Addr'Length < 9 or else
        Addr (Addr'First .. Addr'First + 8) /= "127.0.0.1"
      then
         if User /= S (Username) or else Pwd /= S (Password) then
            --  Small delay to difficult dictionary attacks:
            delay 1.0;
            return False;
         end if;
      end if;

      return True;
   end Check_User;

   -----------------------
   -- Callback function --
   -----------------------
   function Callback_Function (Request : in AWS.Status.Data)
                               return AWS.Response.Data is
      use ASU;
      use Disp_Tables;
      use Templates_Parser;
      URI  : constant String  := Aws.Status.URI (Request);
      I    : constant Cursor  := Find (Dispatchers, URI);
      Path :          Ustring := Root & (+URI);
   begin
      if Embedded then
         Path := +Tail (+Path);
         Log ("Embedded operation: " & Get_Root & URI & " --> " & (+Path),
              Trace.Debug, Log_Section);
      end if;

      declare
         Path : constant String := +Callback_Function.Path;
      begin

         Log ("URI : " & URI,  Trace.Debug, Log_Section);
         Log ("Path: " & Path, Trace.Debug, Log_Section);

         if not Check_User (Request) then
            Log ("Asking auth", Trace.Debug, Log_Section);
            return Authorize (Request);
         end if;

         if AWS.Status.Is_SOAP (Request) then
            Log ("SOAP request", Trace.Debug, Log_Section);
            if Soap_Handler /= null then
               Log ("Found handler for SOAP request", Trace.Debug, Log_Section);
               return Soap_Handler (Request);
            else
               raise Agpl.Exceptions.Unimplemented;
            end if;
         else
            --  Dispatch according to the request:
            if Has_Element (I) then
               Log ("Handler: " & URI, Trace.Debug, Log_Section);
               return Get_Page (Element (I).all, Request);
            elsif Gnat.OS_Lib.Is_Regular_File (Path) or else
              Aws.Resources.Embedded.Is_Regular_File (Path)
            then
               Log ("File: " & Path & "(" & Aws.Mime.Content_Type (Path) & ")" &
                    " as " & Aws.Resources.Embedded.Exists (Path)'Img,
                    Trace.Debug, Log_Section);
               if Aws.Mime.Is_Text (Aws.Mime.Content_Type (Path)) then
                  return Aws.Response.Build
                    (Aws.Mime.Content_Type (Path),
                     Ustring'(Parse (Path, Standard_Xlats (Request))),
                     Cache_Control => Aws.Messages.No_Cache);
               else
                  return Aws.Response.File (Aws.Mime.Content_Type (Path), Path);
               end if;
            else
               Log ("Not found", Trace.Debug, Log_Section);
               return Dispatcher_404 (Request);
            end if;
         end if;
      end;
   exception
      when E : others =>
         Text_Io.Put_Line ("Callback_Function: " & Agpl.Debug.Report (E));
         raise;
   end Callback_Function;

   ------------------------------------------------------------------------
   -- Register_user_pass                                                 --
   ------------------------------------------------------------------------
   --  Stores an user/pass; currently only a pair is stored.
   --  The server must have started with session support.
   procedure Register_User_Pass (User : in String; Pass : in String) is
   begin
      Username := U (User);
      Password := U (Pass);
   end Register_User_Pass;

   ------------------------------------------------------------------------
   -- Register_soap                                                      --
   ------------------------------------------------------------------------
   --  Basic SOAP management: only a custom handler can be registered:
   procedure Register_Soap (Handler : in Handler_Function) is
   begin
      Soap_Handler := Handler;
   end Register_Soap;

   ------------------------------------------------------------------------
   -- Register_handler                                                   --
   ------------------------------------------------------------------------
   procedure Register_Handler
     (URI : in String; Handler : in Handler_Object'Class)
   is
      use Disp_Tables;
   begin
      Insert (Dispatchers, URI, new Handler_Object'Class'(Handler));
      if URI = "" or else URI (URI'First) /= '/' then
         Register_Handler ("/" & URI, Handler);
      end if;
   end Register_Handler;

   ------------------------------------------------------------------------
   -- Request_Redirect                                                   --
   ------------------------------------------------------------------------
   function Request_Redirect (
                              From : in Aws.Status.Data; To : in String) return Aws.Response.Data
   is
   begin
      declare
         New_Req : Aws.Status.Data := From; -- To copy auth data
      begin
         Aws.Status.Set.Request
           (New_Req,
            Aws.Status.GET,
            To,
            Aws.Http_Version);
         return Agpl.Http.Server.Callback_Function (New_Req);
      end;
   end Request_Redirect;

   ------------------------------------------------------------------------
   -- Dispatcher_404                                                     --
   ------------------------------------------------------------------------
   function Dispatcher_404 (Request : in Aws.Status.Data)
                            return Aws.Response.Data
   is
      use Disp_Tables;
      use Templates_Parser;
      I : Cursor := First (Dispatchers);
      V : Vector_Tag;
   begin
      while Has_Element (I) loop
         V := V & Key (I);
         Next (I);
      end loop;
      declare
         Translat : constant Translate_Table :=
           (1 => Assoc ("SERVICE", V),
            2 => Assoc ("VERSION", Server_Name));
      begin
         return Aws.Response.Acknowledge (
           Aws.Messages.S404,
           String'(
             Parse (S (Root) & "err404.html",
               Translat & Standard_Xlats (Request))),
           Aws.Mime.Text_Html);
      end;
   end Dispatcher_404;

   ------------------------------------------------------------------------
   -- Get_root                                                           --
   ------------------------------------------------------------------------
   function Get_Root return String is
   begin
      if Embedded then
         return "";
      else
         return S (Root);
      end if;
   end Get_Root;

   ------------------------------------------------------------------------
   -- Set_root                                                           --
   ------------------------------------------------------------------------
   --  Says what's the root folder to prepend to all requests
   procedure Set_Root (Root : in String := "") is
   begin
      Server.Root := +Replace (Root & "/", "//", "/");
   end Set_Root;

   ------------------------------------------------------------------------
   -- Get_server_name                                                    --
   ------------------------------------------------------------------------
   function Get_Server_Name return String is
   begin
      return S (Server_Name);
   end Get_Server_Name;

   ------------------------------------------------------------------------
   -- Set_server_name                                                    --
   ------------------------------------------------------------------------
   --  String used for the @_VERSION_@ tag
   procedure Set_Server_Name (Version : in String := "") is
   begin
      Server_Name := U (Version);
   end Set_Server_Name;

   ------------------------------------------------------------------------
   -- Set_style_sheet                                                    --
   ------------------------------------------------------------------------
   --  Sets the file to be used as style
   procedure Set_Style_Sheet (Sheet : in String := "") is
   begin
      Style_Sheet := U (Sheet);
   end Set_Style_Sheet;

   ------------------------------------------------------------------------
   -- Get_style_sheet                                                    --
   ------------------------------------------------------------------------
   function Get_Style_Sheet return String is
   begin
      return S (Style_Sheet);
   end Get_Style_Sheet;

   ------------------------------------------------------------------------
   -- Standard_xlats                                                     --
   ------------------------------------------------------------------------
   --  Replaces standard tags for all pages:
   --  VERSION <-- Get_server_name
   --  STYLE   <-- Get_style_sheet
   --  URI     <-- Uri of the request
   function Standard_Xlats (Request : in Aws.Status.Data)
                            return Templates_Parser.Translate_Table is
      use Templates_Parser;
      Params : constant Aws.Parameters.List :=
        Aws.Status.Parameters (Request);
   begin
      return
        (
         Assoc ("STYLE",   Get_Style_Sheet),
         Assoc ("VERSION", Get_Server_Name),
         Assoc ("TIME",    Agpl.Calendar.Format.Timestamp),
         Assoc ("URI",     Aws.Status.URI (Request) &
           Aws.Parameters.Uri_Format (Params))
       );
   end Standard_Xlats;

   ---------------------
   -- Enable_Embedded --
   ---------------------

   procedure Enable_Embedded (Active : Boolean := True) is
   begin
      Embedded := Active;
   end Enable_Embedded;

   ----------------------
   -- Embedded_Enabled --
   ----------------------

   function Embedded_Enabled return Boolean is
   begin
      return Embedded;
   end Embedded_Enabled;

end Agpl.Http.Server;
