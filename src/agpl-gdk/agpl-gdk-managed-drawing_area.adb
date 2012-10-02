with Agpl.Gdk.Palette;
with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

with Gdk.Window; use Gdk.Window;

with Gtk.Handlers; pragma Elaborate_All (Gtk.Handlers);
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object;

package body Agpl.Gdk.Managed.Drawing_Area is

   package Safe_UR is new Standard.Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Safe_Access);

   package Safe_U is new Standard.Gtk.Handlers.User_Callback
     (Gtk_Widget_Record,
      Safe_Access);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Widget : access Gtk_Widget_Record'Class;
                      Event  :        Gdk_Event_Any;
                      This   :        Safe_Access)
   is
      pragma Unreferenced (Widget, Event);
   begin
      This.Set_Destroyed;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Safe_Access.Destroy: " & Report (E),
              Warning);
   end Destroy;

   ------------
   -- Expose --
   ------------

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk_Event_Expose;
                    This   :        Safe_Access)
                    return          Boolean
   is
      pragma Precondition (In_Gtk_Thread);
      pragma Unreferenced (Widget, Event);
   begin
      This.Expose;
      --  No need to do rendez-vous, we're already in the Gtk thread.

      return False;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Safe_Access.Expose: " & Report (E), Warning);
         return False;
   end Expose;

   -------------
   -- Clicked --
   -------------

   function Clicked (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk_Event_Button;
                     This   :        Safe_Access)
                     return Boolean
   is
      pragma Precondition (In_Gtk_Thread);
      pragma Unreferenced (Widget);
   begin
      This.Clicked (Event);

      return False;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Safe_Access.Clicked: " & Report (E), Warning);
         return False;
   end Clicked;

   ---------------
   -- Safe_Code --
   ---------------

   protected body Safe_Code is

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Buffer.Clear;
      end Clear;

      -----------------
      -- Add_Content --
      -----------------

      procedure Add_Content (Draw : Drawing.Drawable'Class) is
      begin
         Draw.Draw (Buffer);
      end Add_Content;

      -----------------
      -- Set_Content --
      -----------------

      procedure Set_Content (Draw : Drawing.Drawable'Class) is
      begin
         Buffer.Clear;
         Draw.Draw (Buffer);
      end Set_Content;

      -------------
      -- Set_Gui --
      -------------

      procedure Set_Gui (Handler : Agpl.Gui.Event_Handler'Class; Replace : Boolean) is
      begin
         if Gui.Is_Valid and then not Replace then
            raise Constraint_Error with "One handler already attached";
            --  We could lift this having a list, but not needed (yet)
         else
            Gui.Set (Handler);
         end if;
      end Set_Gui;

      ----------------
      -- Set_Widget --
      ----------------

      procedure Set_Widget (Area : Gtk_Drawing_Area) is
      begin
         Safe_Code.Gtk_Area := Area;
      end Set_Widget;

      ----------------
      -- Set_Window --
      ----------------

      procedure Set_Window (Window : Gtk_Window) is
      begin
         Safe_Code.Window := Window;
      end Set_Window;

      -------------------
      -- Set_Destroyed --
      -------------------

      procedure Set_Destroyed is
      begin
         Destroyed := True;
      end Set_Destroyed;

      ------------------
      -- Is_Destroyed --
      ------------------

      function Is_Destroyed return Boolean is
      begin
         return Destroyed;
      end Is_Destroyed;

      -----------------
      -- Is_Windowed --
      -----------------

      function Is_Windowed return Boolean is
      begin
         return Window /= null;
      end Is_Windowed;

      -----------------
      -- Set_Bgcolor --
      -----------------

      procedure Set_Bgcolor (Color : String) is
      begin
         Bgcolor := +Color;
      end Set_Bgcolor;

      --------------
      -- Get_Area --
      --------------

      function Get_Area return Gtk_Drawing_Area is
      begin
         return Gtk_Area;
      end Get_Area;

      -----------------
      -- Get_Bgcolor --
      -----------------

      function Get_Bgcolor return String is
      begin
         return +Bgcolor;
      end Get_Bgcolor;

      ------------
      -- Expose --
      ------------

      procedure Expose is
      begin
         if Destroyed then
            Log ("Received exposed when destroyed (?)", Warning, Log_Section);
            return; -- No point in attempting this -- can happen??
         end if;

         if not Real_OK and then Gtk_Area.Realized_Is_Set then
            Real_OK := True;
            Real.Set_Widget (Gtk_Widget (Gtk_Area));
            Real.Set_Drawable (Gtk_Area.Get_Window);

            declare
               Pal : constant Agpl.Gdk.Palette.Object :=
                       Agpl.Gdk.Palette.Create (Gtk_Area.Get_Window);
            begin
               Set_Background (Gtk_Area.Get_Window,
                               Agpl.Gdk.Palette.Get_Color (Pal, Get_Bgcolor));
            end;
         end if;

         if Real_OK then
            Real.Keep_Aspect_Ratio (Square);
            Real.Draw_Begin;
            Buffer.Flush (Real);
            Real.Draw_End;
         else
            Log ("Managed.DA.Safe.Execute: Widget not yet realized",
                 Warning, Log_Section);
         end if;
      exception
         when E : others =>
            Log ("Drawing_Area.Safe.Expose: " & Report (E), Warning);
      end Expose;

      ------------
      -- Redraw --
      ------------

      procedure Redraw is
         pragma Precondition(Managed.In_Gtk_Thread);
      begin
         if not Destroyed then
            Gtk_Area.Queue_Draw;
         end if;
      end Redraw;

      -------------
      -- Clicked --
      -------------

      procedure Clicked (Event : Gdk_Event_Button) is
      begin
         if Gui.Is_Valid and then Real_Ok then
            declare
               use Agpl.Strings;
               Coords : Agpl.Gdk.Float_Vector :=
                          Real.Transform_Back
                            ((Float (Get_X (Event)), Float (Get_Y (Event)), 1.0));
            begin
               Gui.Ref.all.Triggered
                 (Agpl.Gui.Clicked'(Coords (Coords'First), Coords (Coords'First + 1)));
               Log ("Drawing_Area.Clicked:" &
                    To_String (Float (Get_X (Event))) & " " &
                    To_String (Float (Get_Y (Event))),
                    Debug, Log_Section);
               Log ("Drawing_Area.Clicked:" &
                    To_String (Coords (Coords'First)) & " " &
                    To_string (Coords (Coords'First)),
                    Debug, Log_Section);
            end;
         end if;
      end Clicked;

   end Safe_Code;

   ------------------
   -- Prepare_Area --
   ------------------

   procedure Prepare_Area (Safe    : Safe_Access;
                           Bgcolor : String) is
      pragma Precondition (Managed.In_Gtk_Thread);

      Widget : Gtk_Drawing_Area;
   begin
      Gtk.Drawing_Area.Gtk_New (Widget);

      if Safe.Is_Windowed then
         Widget.Set_Size_Request (320, 240);
      else
         Widget.Set_Size_Request (8, 8);
      end if;
      --  This avoids some crash in GTK; user can of course change it at Attach

      Safe.Set_Widget   (Widget);
      Safe.Set_Bgcolor (Bgcolor);

      Widget.Set_Events (Widget.Get_Events or Button_Press_Mask);

      --  Connect the expose event
      Safe_UR.Connect
        (Widget,
         Gtk.Widget.Signal_Expose_Event,
         Safe_UR.To_Marshaller (Expose'Access),
         Safe);

      --  Connect the clicked event
      Safe_UR.Connect
        (Widget,
         Gtk.Widget.Signal_Button_Press_Event,
         Safe_UR.To_Marshaller (Clicked'Access),
         Safe);

      --  Connect the destroy event
      Safe_U.Connect
        (Widget,
         Gtk.Object.Signal_Destroy,
         Safe_U.To_Marshaller (Destroy'Access),
         Safe);

   exception
      when E : others =>
         Log ("Drawing_Area.Prepare_Area [attach]: " & Report (E),
              Error, Log_Section);
   end Prepare_Area;

   ----------
   -- Show --
   ----------

   procedure Show (Draw  : in Drawing.Drawable'Class;
                   Title : in String := "";
                   Bgcolor : in String := "white")
   is
      Throwaway : constant Handle_Access :=
                    new Handle'(Show (Draw, Title, Bgcolor));
      pragma Unreferenced (Throwaway);
   begin
      pragma Memory_Leak ("Throwaway remains in memory forever :S");
      null;
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Draw    : Drawing.Drawable'Class;
                  Title   : String := "";
                  Bgcolor : String := "white";
                  Autogui : Boolean := True) return Handle
   is
      Safe : constant Safe_Access := new Safe_Code (Square => True);

      Window : Gtk_Window;

      -----------------
      -- Show_Window --
      -----------------

      procedure Show_Window is
      begin
         Gtk_New (Window, Window_Toplevel);
         Safe.Set_Window (Window);

         Set_Position (Window, Win_Pos_Center);
         Set_Modal    (Window, True);
         Set_Title    (Window, Title);
         Realize      (Window);

         Prepare_Area (Safe, Bgcolor);

         Window.Add (Safe.Get_Area);

         Show_All (Window);
      end Show_Window;

      H    : constant Handle_Access := new Handle;
      pragma Memory_Leak ("Throwaway remains in memory forever :S");
   begin
      Managed.Execute (Show_Window'Access);

      H.Bind (Safe);
      if Autogui and then Draw in Gui.Event_Handler'Class then
         H.Attach (Gui.Event_Handler'Class (Draw));
      end if;

      return H.all;
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Attach  : access procedure (Widget : Gtk_Widget);
                  Bgcolor : String  := "white";
                  Square  : Boolean := True) return Handle
   is
      Safe   : constant Safe_Access := new Safe_Code (Square);

      procedure Show is
      begin
         Prepare_Area (Safe, Bgcolor);
         Attach       (Gtk_Widget (Safe.Get_Area));
      end Show;

   begin
      Managed.Execute (Show'Access);

      return Handle'(Smart_Safes.Bind (Safe) with null record);
   end Show;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Handle) is
   begin
      This.Ref.all.Clear;
   end Clear;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Handle;
                     Draw :        Drawing.Drawable'Class) is
   begin
      This.Ref.all.Add_Content (Draw);
   end Append;

   ------------
   -- Redraw --
   ------------

   procedure Redraw (This : in out Handle) is
      procedure Inner_Redraw is
      begin
         This.Ref.all.Redraw;
      end Inner_Redraw;
   begin
      --  This must be done this way since there are two locks involved:
      --    our Handle lock and the GtkAda global lock. GtkAda one may or may be
      --    not acquired without the Handle lock (in expose events e.g.).
      --  We must thus always try to acquire the GtkAda lock first.
      Managed.Execute (Inner_Redraw'Access);
      --  (Besides, trying to get to the Gtk thread from within a protected is
      --  a bounded error)
   end Redraw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Handle;
                   What :        Drawing.Drawable'Class)
   is
   begin
      This.Clear;
      This.Append (What);
      This.Redraw;
   end Draw;

   ------------------
   -- Is_Destroyed --
   ------------------

   function Is_Destroyed (This : Handle) return Boolean is
   begin
      return This.Ref.all.Is_Destroyed;
   end Is_Destroyed;

   ------------
   -- Attach --
   ------------

   procedure Attach (This    : in out Handle;
                     Handler :        Gui.Event_Handler'Class;
                     Replace :        Boolean := False) is
   begin
      This.Ref.all.Set_Gui (Handler, Replace);
   end Attach;

end Agpl.Gdk.Managed.Drawing_Area;
