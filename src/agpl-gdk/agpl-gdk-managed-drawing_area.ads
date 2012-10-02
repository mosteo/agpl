--  Used to show some painting, until closed (not for interactive display).
--  The execution is not stopped; that is, a new window is opened in bg.

with Agpl.Drawing;
with Agpl.Drawing.Buffer;
with Agpl.Gdk.Drawer;
with Agpl.Generic_Handle;
with Agpl.Gui;
with Agpl.Smart_Access_Limited;
with Agpl.Ustrings; use Agpl.Ustrings;

with Gdk.Event;    use Gdk.Event;
with Gdk;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;

package Agpl.Gdk.Managed.Drawing_Area is

   pragma Memory_Leak
     ("This package leaks memory: each created area will retain a reference",
      "to the created protected object, even after the area destruction",
      "be it by closing its window or whatever.");

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
   --  No ref to Draw is kept, so it is ok if it goes out of scope afterwards.
   --  The background is parsed according to GTK rules (name, or #0123456)

   function Show (Draw    : Drawing.Drawable'Class;
                  Title   : String  := "";
                  Bgcolor : String  := "white";
                  Autogui : Boolean := True) return Handle;
   --  As above, but provides an updatable object.
   --  If Autogui, check if Draw is a Gui.Event_Handler and attach.

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

   procedure Attach (This    : in out Handle;
                     Handler :        Gui.Event_Handler'Class;
                     Replace :        Boolean := False);
   --  Just clicked for now.
   --  Note that a copy is stored; this means that internal pointers have to
   --    be used in Handler if refs to some other object are needed
   --  If not Replace and already set, error.

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

private

   package Gui_Handles is new
     Agpl.Generic_Handle (Gui.Event_Handler'Class, Gui."=");

   protected type Safe_Code (Square : Boolean) is

      procedure Clear;
      procedure Add_Content (Draw : Drawing.Drawable'Class);
      procedure Set_Content (Draw : Drawing.Drawable'Class);
      procedure Set_Gui     (Handler : Gui.Event_Handler'Class; Replace : Boolean);
      procedure Set_Widget (Area   : Gtk_Drawing_Area);
      procedure Set_Window (Window : Gtk_Window);
      procedure Set_Destroyed;
      procedure Set_Bgcolor (Color : String);

      function Is_Destroyed return Boolean;
      function Is_Windowed  return Boolean; -- if WE manage the top-level window
      function Get_Area     return Gtk_Drawing_Area;
      function Get_Bgcolor  return String;

      procedure Clicked (Event : Gdk_Event_Button);
      procedure Expose;
      procedure Redraw;
   private
      Window    : Gtk_Window; -- Might be null if not stand-alone
      Gtk_Area  : Gtk_Drawing_Area;
      Buffer    : Agpl.Drawing.Buffer.Object;
      Gui       : Gui_Handles.Object;
      Real      : Agpl.Gdk.Drawer.Object;
      Real_OK   : Boolean := False;
      Destroyed : Boolean := False;
      Bgcolor   : UString := +"white";
   end Safe_Code;

   type Safe_Access is access all Safe_Code;

   package Smart_Safes is new Agpl.Smart_Access_Limited
     (Safe_Code, Safe_Access);
   type Smart_Safe is new Smart_Safes.Object with null record;

   type Handle is new Smart_Safe with null record;

   type Handle_Access is access all Handle;

end Agpl.Gdk.Managed.Drawing_Area;
