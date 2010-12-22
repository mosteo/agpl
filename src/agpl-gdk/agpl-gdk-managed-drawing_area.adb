with Agpl.Gdk.Palette;
with Agpl.Monitor;
with Agpl.Trace; use Agpl.Trace;

with Gdk.Event; use Gdk.Event;
with Gdk.Window; use Gdk.Window;

with Gtk.Handlers; pragma Elaborate_All (Gtk.Handlers);
with Gtk.Enums; use Gtk.Enums;

with Ada.Unchecked_Deallocation;

package body Agpl.Gdk.Managed.Drawing_Area is

   Mutex : aliased Monitor.Counting_Semaphore;

   package Handlers_UR is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Object_Access);

   package Safe_UR is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Safe_Access);

   --------------
   -- Drawable --
   --------------

   function Drawable (This : in Draw_Code) return Gdk_Drawable
   is
   begin
      return This.Drawable;
   end Drawable;

   ------------
   -- Widget --
   ------------

   function Widget (This : in Draw_Code) return Gtk_Widget is
   begin
      return This.Widget;
   end Widget;

   -------------
   -- Destroy --
   -------------

   function Destroy (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk_Event_Any;
                     This   :        Object_Access)
                     return          Boolean
   is
      pragma Unreferenced (Widget, Event);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object'Class, Object_Access);

      Alias : Object_Access := This;
   begin
      Free (Alias); -- All allocated memory is gone now. Kwatz!

      return False;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Destroy: " & Report (E), Warning);
         return False;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   function Destroy (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk_Event_Any;
                     This   :        Safe_Access)
                     return          Boolean
   is
      pragma Unreferenced (Widget, Event);
   begin
      This.Set_Destroyed;

      return False;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Safe_Access.Destroy: " & Report (E),
              Warning);
         return False;
   end Destroy;

   ------------
   -- Expose --
   ------------

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk_Event_Expose;
                    This   :        Object_Access)
                    return          Boolean
   is
      pragma Unreferenced (Widget, Event);
   begin
      This.Draw.Drawable := Get_Window (This.Area);
      This.Draw.Widget   := Gtk_Widget (This.Area);
      This.Draw.Execute; --  No need to do rendez-vous, we're already in the Gtk thread.

      return False;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Expose: " & Report (E), Warning);
         return False;
   end Expose;

   ------------
   -- Expose --
   ------------

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk_Event_Expose;
                    This   :        Safe_Access)
                    return          Boolean
   is
      pragma Unreferenced (Widget, Event);
   begin
      This.Execute;
      --  No need to do rendez-vous, we're already in the Gtk thread.

      return False;
   exception
      when E : others =>
         Log ("Managed.Drawing_Area.Safe_Access.Expose: " & Report (E), Warning);
         return False;
   end Expose;

   ----------
   -- Show --
   ----------

   procedure Show (Draw  : in     Draw_Code'Class;
                   Title : in     String := "";
                   Bgcol : in     String := "white")
   is
      M : Monitor.Object (Mutex'Access); pragma Unreferenced (M);

      This : constant Object_Access := new Object;
      --  This is freed in the destroy event

      type Show_Code is new Gtk_Code with null record;
      procedure Execute (X : in out Show_Code) is
         pragma Unreferenced (X);
         Pal : constant Agpl.Gdk.Palette.Object :=
                 Agpl.Gdk.Palette.Create (Draw.Drawable);
      begin
         Set_Title (This.Window, Title);
         Maximize (This.Window);
         Set_Background (Get_Window (This.Area),
                         Agpl.Gdk.Palette.Get_Color (Pal, Bgcol));
         Show_All (This.Window);
      end Execute;

   begin
      This.Draw := new Draw_Code'Class'(Draw);
      --  This is freed at This.Finalize
      declare
         Show_It : Show_Code;
      begin
         Execute_In_Gtk (Show_It);
         --  This creates and shows the window but doesn't draw yet.
         --  The drawing will be triggered at expose event.
      end;
   end Show;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
      type Init_Code is new Gtk_Code with null record;
      procedure Execute (X : in out Init_Code) is
         pragma Unreferenced (X);
      begin
         Gtk_New (This.Window);
         Initialize (This.Window, Window_Toplevel);

         --  Connect the destroy event
         Handlers_Ur.Connect
           (This.Window,
            "delete-event",
            Handlers_Ur.To_Marshaller (Destroy'Access),
            This'Unchecked_Access);

         Set_Position (This.Window, Win_Pos_Center);
         Set_Modal (This.Window, True);
         Realize (This.Window);

         Gtk_New (This.Area);

         --  Connect the expose event
         Handlers_Ur.Connect
           (This.Area,
            "expose-event",
            Handlers_Ur.To_Marshaller (Expose'Access),
            This'Unchecked_Access);

         Add (This.Window, This.Area);
      end Execute;

      Do_It : Init_Code;
   begin
      Execute_In_Gtk (Do_It);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Draw_Code'Class, Access_Code);
   begin
      Free (This.Draw);
   end Finalize;

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

      ----------------
      -- Set_Widget --
      ----------------

      procedure Set_Widget (Widget : Gtk_Widget) is
      begin
         Safe_Code.Widget := Widget;
      end Set_Widget;

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
      -- Set_Bgcolor --
      -----------------

      procedure Set_Bgcolor (Color : String) is
      begin
         Bgcolor := +Color;
      end Set_Bgcolor;

      -----------------
      -- Get_Bgcolor --
      -----------------

      function Get_Bgcolor return String is
      begin
         return +Bgcolor;
      end Get_Bgcolor;

      -------------
      -- Execute --
      -------------

      procedure Execute is
      begin
         if not Real_OK and then Widget.Realized_Is_Set then
            Real_OK := True;
            Real.Set_Widget (Widget);
            Real.Set_Drawable (Widget.Get_Window);

            declare
               Pal : constant Agpl.Gdk.Palette.Object :=
                       Agpl.Gdk.Palette.Create (Widget.Get_Window);
            begin
               Set_Background (Widget.Get_Window,
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
      end Execute;

      ------------
      -- Redraw --
      ------------

      procedure Redraw is
      begin
         Widget.Queue_Draw;
      end Redraw;

   end Safe_Code;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : in out Standard_Code) is
   begin
      This.Safe.Ref.all.Set_Widget (This.Widget);
      This.Safe.Ref.all.Execute;
   end Execute;

   ----------
   -- Show --
   ----------

   procedure Show (Draw  : in Drawing.Drawable'Class;
                   Title : in String := "";
                   Bgcolor : in String := "white")
   is
      Throwaway : constant Handle := Show (Draw, Title, Bgcolor);
      pragma Unreferenced (Throwaway);
   begin
      null;
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Draw    : Drawing.Drawable'Class;
                  Title   : String := "";
                  Bgcolor : String := "white") return Handle
   is
      Sc : Standard_Code;
      --  A copy is made in Show below, so no need to keep anything around.
   begin
      Sc.Safe.Bind (new Safe_Code (True));

      --  Keep a copy of the actions for the expose event
      Sc.Safe.Ref.all.Set_Content (Draw);
      Show (Sc, Title, Bgcolor);

      return Handle'(Sc.Safe with null record);
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Attach  : access procedure (Widget : Gtk_Widget);
                  Bgcolor : String  := "white";
                  Square  : Boolean := True) return Handle
   is
      M : Monitor.Object (Mutex'Access); pragma Unreferenced (M);
      --  This is overcaution?

      Safe   : constant Safe_Access := new Safe_Code (Square);
      Widget :          Gtk_Drawing_Area;

      ----------
      -- Show --
      ----------

      procedure Show is
      begin
         Gtk.Drawing_Area.Gtk_New (Widget);
         Widget.Set_Size_Request (1, 1);
         --  This avoids some crash in GTK; user can of course change it at Attach

         Attach (Gtk_Widget (Widget));
--           Widget.Realize;

         --  Connect the destroy event
         Safe_UR.Connect
           (Widget,
            "delete-event",
            Safe_UR.To_Marshaller (Destroy'Access),
            Safe);

         --  Connect the expose event
         Safe_UR.Connect
           (Widget,
            "expose-event",
            Safe_UR.To_Marshaller (Expose'Access),
            Safe);

         Safe.Set_Widget   (Gtk_Widget (Widget));
         Safe.Set_Bgcolor (Bgcolor);
      exception
         when E : others =>
            Log ("Drawing_Area.Show [attach]: " & Report (E),
                 Error, Log_Section);
      end Show;

   begin
      Execute (Show'Access);

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

end Agpl.Gdk.Managed.Drawing_Area;
