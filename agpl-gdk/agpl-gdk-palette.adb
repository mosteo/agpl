with Glib;       use Glib;
with Gtk.Widget; use Gtk.Widget;

with Agpl.Conversions;
with Agpl.Strings; use Agpl.Strings;
--  with Agpl.Trace;   use Agpl.Trace;

package body Agpl.Gdk.Palette is

   use type Gdk_Drawable;

   ------------------
   -- Set_Drawable --
   ------------------

   procedure Set_Drawable (This : in out Object; X : in Gdk_Drawable) is
   begin
      if This.Drawable /= X and then This.Drawable /= null then
         Finalize (This);
      end if;

      This.Drawable := X;
   end Set_Drawable;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
     (This : access Object;
      Color : in String)
      return Gdk_Color
   is
      use Color_Maps;
      X  : Gdk_Color;
      Ok : Boolean;
      I  : constant Cursor := This.Colors.Find (To_Lower (Color));
   begin
      if Has_Element (I) then
         return Element (I);
      else
         X := Parse (Color);
         Alloc_Color (Get_Default_Colormap, X, Success => Ok); pragma Assert (Ok);
         This.Colors.Insert (To_Lower (Color), X);
         return X;
      end if;
   end Get_Color;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
     (This : access Object;
      Color : in String)
      return Gdk_GC
   is
      use Gc_Maps;
      I : constant Cursor := This.Gcs.Find (To_Lower (Color));
   begin
      if Has_Element (I) then
         return Element (I);
      else
         declare
            C : constant Gdk_Color := This.Get_Color (Color);
            X : Gdk_Gc;
         begin
            Gdk_new (X, This.Drawable);
            Set_Foreground (X, C);
            This.Gcs.Insert (To_Lower (Color), X);
            return X;
         end;
      end if;
   end Get_Color;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Gc (This       : access Object;
                    Color      : in     String;
                    Line_Width : in     Integer        := 1;
                    Line_Style : in     Gdk_Line_Style := Line_Solid)
                    return              Gdk_Gc
   is
      use Gc_Maps;
      Key : constant String := To_Lower (Color) & ":" &
                               Line_Width'Img & ":" &
                               Line_Style'Img;
      I : constant Cursor := This.Gcs.Find (Key);
   begin
      if Has_Element (I) then
         return Element (I);
      else
         declare
            C : constant Gdk_Color := This.Get_Color (Color);
            X : Gdk_Gc;
         begin
            Gdk_new (X, This.Drawable);
            Set_Foreground (X, C);
            Set_Line_Attributes (X,
                                 Gint (Line_Width),
                                 Line_Style,
                                 Cap_Round,
                                 Join_Miter);
            This.Gcs.Insert (Key, X);
            return X;
         end;
      end if;
   end Get_GC;

   function Get_Color (This : in Object; Color : in String) return Gdk_Color is
   begin
      return Get_Color (This'Unrestricted_Access, Color);
   end Get_Color;

   function Get_Color (This : in Object; Color : in String) return Gdk_GC is
   begin
      return Get_Color (This'Unrestricted_Access, Color);
   end Get_Color;

   --------------------
   -- Get_Color_Name --
   --------------------

   function Get_Color_Name (R, G, B : Rgb_Component) return String is
      use Agpl.Conversions;
   begin
      return "#" & To_Hex (R, 2) & To_Hex (G, 2) & To_Hex (B, 2);
   end Get_Color_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
      pragma Unreferenced (This);
   begin
      --  Log ("At initizalize", Always);
      null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Object) is
      procedure Update (X : in Gc_Maps.Cursor) is
      begin
         Ref (Gc_Maps.Element (X));
      end Update;
   begin
      --  Log ("Adjust: Pal has #GC = " & This.Gcs.Length'Img, Always);
      Gc_Maps.Iterate (This.Gcs, Update'Access);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
      procedure Free (X : in Gc_Maps.Cursor) is
      begin
         Unref (Gc_Maps.Element (X));
      end Free;
   begin
      --  Log ("Finalize: Pal has #GC = " & This.Gcs.Length'Img, Always);
      Gc_Maps.Iterate (This.Gcs, Free'Access);
      Gc_Maps.Clear (This.Gcs);
      Color_Maps.Clear (This.Colors);
   end Finalize;

end Agpl.Gdk.Palette;
