--  Decorators and predefined instances to have them at library level:

with Agpl.Trace.File;

package Agpl.Trace.Utils is

   pragma Elaborate_Body;

   Filer : aliased File.Object;

   --  This enables last-chance handler & symbolic tracebacks
   procedure Enable_Symbolic_Tracebacks;

   --  Prependers:

   function Prepend_Level (Text    : in String;
                           Level   : in Levels;
                           Section : in String) return String;
   --  Adds a marker of message level
   pragma Inline (Prepend_Level);

   function Prepend_Timestamp (Text    : in String;
                               Level   : in Levels;
                               Section : in String) return String;
   --  Add a timestamp.
   pragma Inline (Prepend_Timestamp);

   function Prepend_Level_Timestamp (Text    : in String;
                                     Level   : in Levels;
                                     Section : in String) return String;
   --  Add level & timestamp
   pragma Inline (Prepend_Level_Timestamp);

   function Prepend_Level_Timestamp_Section (Text    : in String;
                                             Level   : in Levels;
                                             Section : in String) return String;
   pragma Inline (Prepend_Level_Timestamp_Section);

end Agpl.Trace.Utils;
