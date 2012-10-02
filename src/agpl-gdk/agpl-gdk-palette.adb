with Glib;       use Glib;
with Gtk.Widget; use Gtk.Widget;

with Agpl.Conversions;
with Agpl.Scaling1d;
with Agpl.Strings; use Agpl.Strings;
with Agpl.Trace;   use Agpl.Trace;

package body Agpl.Gdk.Palette is

   use type Gdk_Drawable;

   ------------
   -- Create --
   ------------

   function Create (Gd : Gdk_Drawable) return Object is
   begin
      return This : Object do
         This.Drawable := Gd;
      end return;
   end Create;

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
   exception
      when others =>
         Log ("Get_Color failed for color: " & Color, Error, Log_Section);
         raise;
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
            C : constant Gdk_Color := Get_Color (This, Color);
            X : Gdk_GC;
         begin
            Gdk_New (X, This.Drawable);
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
                    return              Gdk_GC
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
            C : constant Gdk_Color := Get_Color (This, Color);
            X : Gdk_GC;
         begin
            Gdk_New (X, This.Drawable);
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
   end Get_Gc;

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
      return "#" &
      To_Hex (Natural (R), 2) &
      To_Hex (Natural (G), 2) &
      To_Hex (Natural (B), 2);
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

   --------------
   -- Gradient --
   --------------

   function Gradient (Val : Domain_Range'Base) return Rgb_Component is
      subtype LLF is Long_Long_Float;
      subtype LLI is Long_Long_Integer;
      subtype RGB is LLI range LLI (Rgb_Component'First) .. LLI (Rgb_Component'Last);
      package Scale is new Scaling1d (LLF);
      S : constant Scale.Object :=
            Scale.Set_Equivalence (LLF (Domain_Range'First), LLF (Domain_Range'Last),
                                   LLF (Rgb_Min), LLF (Rgb_Max));
   begin
      return Rgb_Component
        (RGB'Base'Max (RGB'First,
                       RGB'Base'Min (RGB'Last,
                                     RGB'Base (S.Scale (LLF (Val))))));
   end Gradient;

   ------------------
   -- Get_Gradient --
   ------------------

   function Get_Gradient (Val_In_Domain                : Float;
                          Min_In_Domain, Max_In_Domain : Float;
                          Min_In_Rgb,    Max_In_Rgb    : Rgb_Component)
                          return Rgb_Component
   is
      Val : Float renames Val_In_Domain;
      subtype LLF is Float;
      subtype LLI is Long_Long_Integer;
      subtype RGB is LLI range LLI (Rgb_Component'First) .. LLI (Rgb_Component'Last);
      package Scale is new Scaling1d (LLF);
      S : constant Scale.Object :=
            Scale.Set_Equivalence (LLF (Min_In_Domain), LLF (Max_In_Domain),
                                   LLF (Min_In_Rgb), LLF (Max_In_Rgb));
   begin
      return Rgb_Component
        (RGB'Base'Max (RGB'First,
                       RGB'Base'Min (RGB'Last,
                                     RGB'Base (S.Scale (LLF (Val))))));
   end Get_Gradient;

end Agpl.Gdk.Palette;
