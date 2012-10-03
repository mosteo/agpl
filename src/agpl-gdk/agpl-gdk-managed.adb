with Ada.Task_Identification;
with Agpl.Chronos; pragma Elaborate_All (Agpl.Chronos);
with Agpl.Trace; use Agpl.Trace;
with Gdk.Threads;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with System;

package body Agpl.Gdk.Managed is

   use Widget_List;

   Started : Boolean := False;
   pragma Atomic (Started);

   Gtk_Thread_Id : Ada.Task_Identification.Task_Id :=
                     Ada.Task_Identification.Null_Task_Id;
   pragma Atomic (Gtk_Thread_Id);

   -------------------
   -- In_Gtk_Thread --
   -------------------

   function In_Gtk_Thread return Boolean is
      use Ada.Task_Identification;
   begin
      return Ada.Task_Identification.Current_Task = Gtk_Thread_Id;
   end In_Gtk_Thread;

   task Gtk_Task is

      entry Start;

      entry Execute (This : in out Gtk_Code'Class; OK : out Boolean);
      --  Dispatch on This.Execute inside the Gtk thread.

   end Gtk_Task;

   -----------------------
   -- Glade_Autoconnect --
   -----------------------

--     procedure Glade_Autoconnect (Xml : Glade.XML.Glade_XML) is
--        procedure Glade_Connect (X : System.Address);
--        pragma Import (C, Glade_Connect, "glade_xml_signal_autoconnect");
--        procedure Internal is
--        begin
--           Glade_Connect (Xml.Get_Object);
--        end Internal;
--     begin
--        Execute (Internal'Access);
--     end Glade_Autoconnect;

   procedure GtkBuilder_Connect (Builder : Gtkada_Builder;
                                 Data    : access User_Data) is
      procedure Internal (Builder :        System.Address;
                          Data    : access User_Data) with
        Import,
        Convention => C,
        External_Name => "gtk_builder_connect_signals";
   begin
      Internal (Builder.all'Address, Data);
      pragma Untested ("The above line is most likely a bomb");
   end GtkBuilder_Connect;

   procedure GtkBuilder_Connect_Void (Builder : Gtkada_Builder) is
      type Void is null record;
      procedure Internal is new GtkBuilder_Connect (Void);
   begin
      Internal (Builder, null);
   end GtkBuilder_Connect_Void;

   --------------------
   -- Execute_In_Gtk --
   --------------------

   procedure Execute_In_Gtk (This : in out Gtk_Code'Class) is
      use type Ada.Task_Identification.Task_Id;
      OK : Boolean;
   begin
      while not Started loop
         delay 0.1;
      end loop;

      --  Avoid re-entering task and thus deadlock.
      if Ada.Task_Identification.Current_Task = Gtk_Thread_Id then
         Log ("Executing in current thread...", Debug, Det_Section);
         This.Execute;
         Log ("Executing in current thread... DONE", Debug, Det_Section);
      else
         Log ("Executing in GTK thread...", Debug, Det_Section);
         select
            Gtk_Task.Start;
         else
            null;
         end select;

         Gtk_Task.Execute (This, OK);
         Log ("Executing in GTK thread... DONE", Debug, Det_Section);
         if not OK then
            raise Program_Error with "GTK execution was aborted";
         end if;
      end if;
   exception
      when E : others =>
         Log ("Managed.Execute_in_Gtk: " & Report (E), Error, Log_Section);
   end Execute_In_Gtk;

   -------------
   -- Execute --
   -------------

   procedure Execute (Code : access procedure) is
      type Local_Code (Code : access procedure) is
        new Gtk_Code with null record;
      procedure Execute (X : in out Local_Code) is
      begin
         X.Code.all;
      end Execute;

      L : Local_Code (Code);
   begin
      Execute_In_Gtk (L);
   end Execute;

   --------------
   -- Gtk_Task --
   --------------

   task body Gtk_Task is

      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         --  Standard.Gtk.Main.Set_Locale; -- Was obsolescent
         Standard.Gtk.Main.Init;
         Standard.Gdk.Threads.G_Init;
         Standard.Gdk.Threads.Init;
      end Init;

      ---------------------
      -- Event_Iteration --
      ---------------------

      procedure Event_Iteration is
      begin
         Standard.Gdk.Threads.Enter;
         while Standard.Gtk.Main.Events_Pending loop
            begin
               if Standard.Gtk.Main.Main_Iteration then
                  null;
               end if;
            exception
               when E : others =>
                  Log ("Managed.Gtk_Task.Event_Iteration: " & Report (E),
                       Error, Log_Section);
            end;
         end loop;
         Standard.Gdk.Threads.Leave;
      end Event_Iteration;

   begin
      Started := True;
      Gtk_Thread_Id := Ada.Task_Identification.Current_Task;

      select
         accept Start;
      or
         terminate;
      end select;

      Init;
      Log ("Gtk_Task [managed]: Running...", Debug, Log_Section);

      loop
         begin
            --  Execute codes
            select
               accept Execute (This : in out Gtk_Code'Class;
                               OK   :    out Boolean) do
                  Standard.Gdk.Threads.Enter;
                  select
                     delay 10.0;
                     Log ("Gtk_Task: Aborted managed code (too long): " &
                          External_Tag (This'Tag),
                          Warning, Log_Section);
                     OK := False;
                  then abort
                     declare
                        Timer : Agpl.Chronos.Object;
                     begin
                        This.Execute;
                        OK := True;
                        if Timer.Elapsed > 5.0 then
                           Log ("Managed.Gtk_Task.Execute: Long processing: " &
                                Timer.Image, Warning, Log_Section);
                        end if;
                     exception
                        when E : others =>
                           Log ("Managed.Gtk_Task.Execute: " & Report (E),
                                Error, Log_Section);
                     end;
                  end select;
                  Standard.Gdk.Threads.Leave;
               end Execute;
            or
               delay 0.01;
            end select;

            --  Process events
            Event_Iteration;

         exception
            when E : others =>
               Log ("Gtk_Task.Event_Iteration: " & Report (E),
                    Error, Log_Section);
               delay 0.01; -- Prevent possible CPU hog.
         end;
      end loop;
   end Gtk_Task;

end Agpl.Gdk.Managed;
