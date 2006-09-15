with Agpl.Trace; use Agpl.Trace;

with Gtk.Main; pragma Elaborate_All (Gtk.Main);
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

--  with Ada.Tags; use Ada.Tags;
--  with Ada.Text_Io; use Ada.Text_Io;

package body Agpl.Gdk.Managed is

   use Widget_List;

   task Gtk_Task is

      entry Execute (This : in out Gtk_Code'Class);
      --  Dispatch on This.Execute inside the Gtk thread.

   end Gtk_Task;

   --------------------
   -- Execute_In_Gtk --
   --------------------

   procedure Execute_In_Gtk (This : in out Gtk_Code'Class) is
   begin
      Gtk_Task.Execute (This);
   end Execute_In_Gtk;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      null;
   end Start;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      null;
   end Shutdown;

   --------------
   -- Gtk_Task --
   --------------

   task body Gtk_Task is

      function Num_Windows return Natural is
      begin
         return Integer'Max (Integer (Length (List_Toplevels)) - 1, 0);
      end Num_Windows;

   begin
      Gtk.Main.Init;

      loop
         begin
            if Num_Windows = 0 then
               --  Execute codes
               select
                  accept Execute (This : in out Gtk_Code'Class) do
                     select
                        delay 5.0;
                        Log ("Gtk_Task: Aborted managed code (too busy)", Warning);
                     then abort
                        Managed.Execute (This);
                     end select;
                  exception
                     when E : others =>
                        Log ("Gtk_Task: In managed code: " & Report (E), Error);
                  end Execute;
               or
                  terminate;
               end select;
            else
               select
                  accept Execute (This : in out Gtk_Code'Class) do
                     select
                        delay 5.0;
                        Log ("Gtk_Task: Aborted managed code (too busy)", Warning);
                     then abort
                        Managed.Execute (This);
                     end select;
                  exception
                     when E : others =>
                        Log ("Gtk_Task: In managed code: " & Report (E), Error);
                  end Execute;
               or
                  delay 0.01;
               end select;
            end if;

            --  Process events
            declare
               Dummy : Boolean;
               pragma Unreferenced (Dummy);
            begin
               while Gtk.Main.Events_Pending loop
                  Dummy := Gtk.Main.Main_Iteration (Blocking => False);
               end loop;
            end;

         exception
            when E : others =>
               Log ("Gtk_Task: " & Report (E), Error);
               delay 0.01; -- Prevent possible CPU hog.
         end;
      end loop;
   end Gtk_Task;

end Agpl.Gdk.Managed;
