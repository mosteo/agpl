with Agpl.Text_Io;

package body Agpl.Trace.Console is

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "")
   is
   begin
      if This.Must_Log (Level, Section) then
         Agpl.Text_Io.Put_Line (This.Decorate (Text, Level, Section));
         Root.Object (This).Log (Text, Level, Section);
      end if;
   end Log;

end Agpl.Trace.Console;
