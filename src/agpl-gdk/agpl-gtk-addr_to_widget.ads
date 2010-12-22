with Glib.Object;
with System;

--  Convenience functions to obtain a proper widget from a C *object
--  Added on demand...

--  There's no memory leak on each new Ada object because somehow GtkAda
--  is always returning the same pointer (!?)

generic
   type Stub   is        new Glib.Object.GObject_Record with private;
   type Target is access all Stub'Class;
function Agpl.Gtk.Addr_To_Widget (Addr : System.Address) return Target;
