--  Hierarchy starting here provides some utilities for displaying drawings
--  without having to take care of any Gdk/Gtk management. In other words,
--  a new thread for Gtk is created, and all Gtk related code is called from
--  within.

with Gtkada.Builder; use Gtkada.Builder;

package Agpl.Gdk.Managed is

   --  pragma Elaborate_Body;

   Log_Section : constant String := "agpl.gdk.managed";
   Det_Section : constant String := "agpl.gdk.managed.detail";

   --  IMPORTANT:
   --  All calls to Gdk/Gtk must be performed from a same thread, managed
   --  here. To do that, you can use the functions here. These functions will
   --  properly check that you're not already in the Gtk thread, so you *must*
   --  err in the safe side and use them, always.

   procedure Execute (Code : access procedure);
   --  Execute this code within the main GTK thread
   --  Will initialize first time called

   --  procedure Glade_Autoconnect (Xml : Glade.XML.Glade_XML);
   generic
      type User_Data is limited private; -- A ptr to this is give to GtkBuilder
   procedure GtkBuilder_Connect (Builder : Gtkada_Builder;
                                 Data    : access User_Data);
   --  Remember to compile with -rdynamic (linker, C++) in order for this
   --  to find the procedures.

   procedure GtkBuilder_Connect_Void (Builder : Gtkada_Builder);
   --  Use this when no user data ptr is needed

   --  OO-mindwnking

   type Gtk_Code is abstract tagged null record;
   --  This type must be extended to provide code to be executed.
   --  Note that the type is not limited. Any info required for drawing must
   --  be accessible at all times to the instance, since now the drawing is
   --  decoupled from other code flow.

   procedure Execute (This : in out Gtk_Code) is abstract;
   --  Override this with the code required.

   --  For lazyness:
   type Null_Code is new Gtk_Code with null record;

   procedure Execute (This : in out Null_Code) is null;

   function In_Gtk_Thread return Boolean;
   pragma Inline (In_Gtk_Thread);
   --  Says if we're already in the Gtk thread

private

   procedure Execute_In_Gtk (This : in out Gtk_Code'Class);
   --  Non dispatching. To be used by children of Managed to execute GTK code.

end Agpl.Gdk.Managed;
