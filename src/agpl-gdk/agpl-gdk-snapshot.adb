with Agpl.Trace; use Agpl.Trace;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib;       use Glib;
with Glib.Error; use Glib.Error;

package body Agpl.Gdk.Snapshot is

   --------------
   -- Save_Png --
   --------------

   procedure Save_Png (This : Gdk_Drawable; Filename : String) is
      Buffer : Gdk_Pixbuf;
      Error  : GError;
      W, H   : Gint;
   begin
      Get_Size (This, Width => W, Height => H);

      Buffer := Get_From_Drawable (null,
                                   This,
                                   Get_Colormap (This),
                                   0, 0, 0, 0,
                                   W - 1,
                                   H - 1);
      Save (Buffer,
            Filename,
            PNG,
            Error);

      if Error /= null then
         Log ("Error saving to " & Filename, Warning);
      end if;

      Unref (Buffer);
   end Save_Png;

end Agpl.Gdk.Snapshot;
