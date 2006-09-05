with Agpl.Trace; use Agpl.Trace;

with Gtk.Main; pragma Elaborate_All (Gtk.Main);

--  with Ada.Tags; use Ada.Tags;
  with Ada.Text_Io; use Ada.Text_Io;

package body Agpl.Gdk.Managed is

   Started : Boolean := False;

   task Gtk_Task is

      entry Start;

      entry Execute (This : in out Gtk_Code'Class);
      --  Dispatch on This.Execute inside the Gtk thread.

      entry Shutdown;

   end Gtk_Task;

   --------------------
   -- Execute_In_Gtk --
   --------------------

   procedure Execute_In_Gtk (This : in out Gtk_Code'Class) is
   begin
      if not Started then
         raise Program_Error;
      else
         Gtk_Task.Execute (This);
      end if;
   end Execute_In_Gtk;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Gtk_Task.Start;
      Started := True;
   end Start;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Gtk_Task.Shutdown;
   end Shutdown;

   --------------
   -- Gtk_Task --
   --------------

   task body Gtk_Task is
      Done : Boolean := False;
   begin
      select
         accept Start;
      or
         accept Shutdown;
         Done := True;
      or
         terminate;
      end select;

      if not Done then
         Gtk.Main.Init;
      end if;

      while not Done loop
         begin
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
               accept Shutdown;
               Done := True;
               --  Gtk.Main.Main_Quit;
               --  No need to leave, since we're not in a blocking loop.
            or
               delay 0.01;
            end select;

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
