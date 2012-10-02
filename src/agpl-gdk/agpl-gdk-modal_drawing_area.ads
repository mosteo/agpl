--  Used to show some painting, until closed (not for interactive display).

with Agpl.Generic_Handle;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Window;       use Gtk.Window;

with Ada.Finalization;

package Agpl.Gdk.Modal_Drawing_Area is

   --  pragma Elaborate_Body;

   type Object is tagged limited private;

   type Callback is abstract tagged null record;

   procedure Draw (C        : in out Callback;
                   Drawable :        Gdk_Drawable) is abstract;

   procedure Show (This  : in out Object;
                   Draw  :        Callback'Class;
                   Title : in     String := "");
   --  Will call Draw on "expose" event.
   --  Display anything that has been drawn.
   --  Will not return until the window is closed.

private

   package Cb_Handles is new Agpl.Generic_Handle (Callback'Class);

   type CB_Handle is new Cb_Handles.Object with null record;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Area   : Gtk_Drawing_Area;
      Window : Gtk_Window;
      Draw   : CB_Handle;
   end record;

   type Object_Access is access all Object'Class;

   procedure Initialize (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Gdk.Modal_Drawing_Area;
