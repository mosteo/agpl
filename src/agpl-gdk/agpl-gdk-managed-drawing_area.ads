--  Used to show some painting, until closed (not for interactive display).
--  The execution is not stopped; that is, a new window is opened in bg.

with Agpl.Drawing;
with Agpl.Drawing.Buffer;
with Agpl.Gdk.Drawer;
with Agpl.Smart_Access_Limited;
with Agpl.Ustrings; use Agpl.Ustrings;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;

with Ada.Finalization;

package Agpl.Gdk.Managed.Drawing_Area is

   type Handle is tagged private;
   --  Needed only if we intend to perform updates.
   --  This type is thread safe, so you can copy and pass it around.
   --  It is however pointless to use it after the widget has been
   --    closed/destroyed, nothing will happen (although no error should occur).
   --  At that point the only reference (this is a smart pointer in the end) will
   --    be the client's one, so it should be disposed of to free any lingering
   --    claimed memory.
   --  This all is untested, so some leak is likely to remain.

   procedure Show (Draw    : in Drawing.Drawable'Class;
                   Title   : in String := "";
                   Bgcolor : in String := "white");
   --  This is the recommended usage... below is the old way of doing things.
   --  No copy of Draw is kept, so it is ok if it goes out of scope afterwards.
   --  The background is parsed according to GTK rules (name, or #0123456)

   function Show (Draw    : Drawing.Drawable'Class;
                  Title   : String := "";
                  Bgcolor : String := "white") return Handle;
   --  As above, but provides an updatable object.

   function Show (Attach  : access procedure (Widget : Gtk_Widget);
                  Bgcolor : String  := "white";
                  Square  : Boolean := True) return Handle;
   --  Square refers to keeping aspect ratio.
   --  This procedure doesn't create a window.
   --  Instead, Attach is called within the GTK thread, with the widget just
   --    created as parameter. There, you can embed it in some container, and
   --    possibly associate it to some Agpl.Drawing.Drawer'Class.
   --  However, it's probably better to use the Clear/Append/Redraw below...
   --  Also you can there set any specifics about size, packing, etc.
   --  Note that there's no drawable for the Widget yet, since it is known after
   --    being attached to one...

   procedure Clear (This : in out Handle);
   --  Erase contents.
   --  This won't trigger a redraw, in order to avoid flicker.

   procedure Append (This : in out Handle;
                     Draw :        Drawing.Drawable'Class);
   --  Add something to be painted but keeping existing content.

   procedure Redraw (This : in out Handle);
   --  Force a repaint ASAP. To be used after several appends, most likely.

   procedure Draw (This : in out Handle;
                   What :        Drawing.Drawable'Class);
   --  Clear + Append + Redraw, all in one

   function Is_Destroyed (This : Handle) return Boolean;
   --  Check for RIPness

   -----------------------
   --  OBSOLESCENT WAY  --
   -----------------------

   type Draw_Code is abstract new Gtk_Code with private;
   --  This type is used by the user to supply the drawing code in its Execute.

   function Drawable (This : in Draw_Code) return Gdk_Drawable;
   --  The user must use this in Execute to get the drawable.

   function Widget (This : in Draw_Code) return Gtk_Widget;
   --  The user must use this in Execute to get the widget containing the drawable.
   --  For example to get pango layouts.

   procedure Show (Draw  : in     Draw_Code'Class;
                   Title : in     String := "";
                   Bgcol : in     String := "white");
   --  A copy of Draw will be kept, so there shouldn't be references to external
   --  data that may have dissapeared.

private

   type Draw_Code is abstract new Gtk_Code with record
      Drawable : Gdk_Drawable;
      Widget   : Gtk_Widget;
   end record;
   --  This type is a remainder of the old way of doing things.
   --  It's still used internally by the new machinery, though, so I can't
   --    remove it -- yet.

   type Access_Code is access all Draw_Code'Class;

   --  These types below rely still in Draw_Code, but are used to exempt
   --  the client from using Draw_Code at all, and instead use Drawable'Class
   protected type Safe_Code (Square : Boolean) is
      procedure Clear;
      procedure Add_Content (Draw : Drawing.Drawable'Class);
      procedure Set_Content (Draw : Drawing.Drawable'Class);
      procedure Set_Widget (Widget : Gtk_Widget);
      procedure Set_Destroyed;
      procedure Set_Bgcolor (Color : String);

      function Is_Destroyed return Boolean;
      function Get_Bgcolor return String;

      procedure Execute;
      procedure Redraw;
   private
      Widget   : Gtk_Widget;
      Buffer   : Agpl.Drawing.Buffer.Object;
      Real     : Agpl.Gdk.Drawer.Object;
      Real_OK  : Boolean := False;
      Destroyed : Boolean := False;
      Bgcolor  : Ustring;
   end Safe_Code;

   type Safe_Access is access all Safe_Code;

   package Smart_Safes is new Agpl.Smart_Access_Limited
     (Safe_Code, Safe_Access);
   type Smart_Safe is new Smart_Safes.Object with null record;

   type Handle is new Smart_Safe with null record;

   --  This is the type that we use to remove the client necessity for Draw_Code
   --  Also, this is only needed when the drawable is in a standalone window
   type Standard_Code is new Draw_Code with record
      Safe : Smart_Safe;
   end record;

   overriding
   procedure Execute (This : in out Standard_Code);

   type Object is new Ada.Finalization.Limited_Controlled with record
      Area   : Gtk_Drawing_Area;
      Window : Gtk_Window;
      Draw   : Access_Code;
   end record;

   type Object_Access is access all Object'Class;

   procedure Initialize (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Gdk.Managed.Drawing_Area;
