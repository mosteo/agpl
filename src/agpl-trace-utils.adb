with Agpl.Calendar.Format;
with Agpl.Strings;

with GNAT.Exception_Traces;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

package body Agpl.Trace.Utils is

   subtype Warn_Prefix is String (1 .. 4);
   type Prefixes is array (All_levels) of Warn_Prefix;
   Prefix : constant Prefixes := (
      Never       => ":n: ",
      Debug       => "-d- ",
      Informative => "(i) ",
      Error       => "[E] ",
      Warning     => "<w> ",
      Always      => "!A! ");

      --------------------------------
   -- Enable_Symbolic_Tracebacks --
   --------------------------------

   procedure Enable_Symbolic_Tracebacks is
   begin
      --  For unhadled exceptions: (Using for every raise causes printing even if handled, so not useful)
      GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);
      GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   end Enable_Symbolic_Tracebacks;

   -------------------
   -- Prepend_Level --
   -------------------

   function Prepend_Level
     (Text    : in String;
      Level   : in Levels;
      Section : in String)
      return String
   is
      pragma Unreferenced (Section);
   begin
      return Prefix (Level) & Text;
   end Prepend_Level;

   -----------------------
   -- Prepend_Timestamp --
   -----------------------

   function Prepend_Timestamp
     (Text    : in String;
      Level   : in Levels;
      Section : in String)
      return String
   is
      pragma Unreferenced (Level, Section);
   begin
      return " [" &Calendar.Format.Timestamp & "] " & Text;
   end Prepend_Timestamp;

   -----------------------------
   -- Prepend_Level_Timestamp --
   -----------------------------

   function Prepend_Level_Timestamp
     (Text    : in String;
      Level   : in Levels;
      Section : in String)
      return String
   is
      pragma Unreferenced (Section);
   begin
      return
        Prefix (Level) &
        "[" &Calendar.Format.Timestamp & "] " & Text;
   end Prepend_Level_Timestamp;

   -------------------------------------
   -- Prepend_Level_Timestamp_Section --
   -------------------------------------

   function Prepend_Level_Timestamp_Section (Text    : in String;
                                             Level   : in Levels;
                                             Section : in String) return String
   is
      Section_Width : constant := 16;
      use Strings;
   begin
      if Section'Length <= Section_Width then
         return
           Prefix (Level) &
         "[" & Calendar.Format.Timestamp & "]" &
         "[" & Lpad (Section, Section_Width) & "] " &
         Text;
      else
         return
           Prefix (Level) &
         "[" & Calendar.Format.Timestamp & "]" &
         "[.." & Section (Section'Last - Section_Width + 3 ..Section'Last) & "] " &
         Text;
      end if;
   end Prepend_Level_Timestamp_Section;

end Agpl.Trace.Utils;
