with Agpl.Trace; use Agpl.Trace;

with Gtk.Main; pragma Elaborate_All (Gtk.Main);
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

--  with Ada.Tags; use Ada.Tags;
--  with Ada.Text_Io; use Ada.Text_Io;

package body Agpl.Gdk.Managed is

   use Widget_List;

   task Gtk_Task is

      entry Start;

      entry Execute (This : in out Gtk_Code'Class);
      --  Dispatch on This.Execute inside the Gtk thread.

   end Gtk_Task;

   --------------------
   -- Execute_In_Gtk --
   --------------------

   procedure Execute_In_Gtk (This : in out Gtk_Code'Class) is
   begin
      select
         Gtk_Task.Start;
      else
         null;
      end select;
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

      Windows_At_Start : constant Integer := 2;
      --  I'm not sure what happens. Once I create my first window, the toplevel
      --  count jumps from 0 to 3, so there are two hidden ones I don't know.
      --  In previous versions of GtkAda they were just 1!

      function Num_Windows return Natural is
      begin
         return
           Integer'Max (Integer (Length (List_Toplevels)) - Windows_At_Start, 0);
      end Num_Windows;

   begin
      select
         accept Start;
      or
         terminate;
      end select;

      Gtk.Main.Init;
      Log ("Gtk_Task [managed]: Running...", Informative);

      loop
         begin
            --  Put_Line (Num_Windows'Img);
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
