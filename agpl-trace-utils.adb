with Agpl.Calendar.Format;

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

end Agpl.Trace.Utils;
