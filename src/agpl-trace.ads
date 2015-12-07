with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Finalization;
with Agpl.Trace_Is;

with Ada.Exceptions;
with Ada.Tags;         use Ada.Tags;
use Ada;

--  Root for logging facilities
--  By default, a console logger is created. If you don't want it, disable it
--  via the access that you can get with the function Console_Tracer

package Agpl.Trace is

   pragma Preelaborate;

   Enabled : Boolean renames Trace_Is.Enabled;
   --  Inlining shall cause no calls when this is false.
   --  I have tested this; if this doesn't work something has gone wrong.

   Master_Switch : constant Boolean := False;
   --  when this is true, *EVERYTHING* will be traced.

   --  This object is not thread safe.
   --  type Object is limited interface;
   type Object is abstract tagged limited private; -- null record;
   --  Should be an interface but the gnat bug with containers prevents it
   type Object_Access is access all Object'Class;

   pragma Preelaborable_Initialization (Object);

   type Levels is (Never,
                   Debug,
                   Informative,
                   Warning,
                   --  From here up, always logged even if section not enabled
                   Error,
                   Always);

   for Levels use (Never       => 0,
                   Debug       => 1,
                   Informative => 2,
                   Warning     => 3,
                   Error       => 4,
                   Always      => 5);

   subtype All_Levels     is Levels     range Never .. Always;
   subtype Warning_levels is All_Levels range Debug .. Error;

   type Decorator is access function (Text    : in String;
                                      Level   : in Levels;
                                      Section : in String) return String;

   Null_Object : constant Object_Access := null;

   procedure Enable_Section  (This    : in out Object;
                              Section : in     String;
                              Enabled : in     Boolean := True)
   is abstract;

   procedure Enable_Section (This    : in out Object;
                             Section : String;
                             Level   : All_Levels) is abstract;
   --  Section has its own level definition

   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "") is abstract;

   procedure Log
     (This    : in Object_Access;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "");
   --  In purpose, This can be null to allow the passing of Null_Object.
   --  This call *is* thread safe
   pragma Inline (Log);

   procedure Set_Level (This : in out Object; Level : in All_Levels)
   is abstract;
   --  Minimum for a message to be logged.

   procedure Set_Active (This : in out Object; Active : in Boolean := True)
   is abstract;

   procedure Set_Decorator (This : in out Object; Decor : in Decorator)
   is abstract;

   function Decorate (This    : in Object;
                      Text    : in String;
                      Level   : in Levels;
                      Section : in String) return String
   is abstract;
   --  Apply the decorator.

   function Must_Log (This    : in Object;
                      Level   : in Levels;
                      Section : in String) return Boolean
   is abstract;

   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "");
   --  This call *Is* thread safe.
   pragma Inline (Log);

   function Would_Log (Level : Levels; Section : String := "") return Boolean;
   --  Says if some tracer would log with this setup.
   --  Allows to know in advance if some trace should be prepared

   procedure Add_Tracer (This : not null Object_Access);
   --  Add a tracer that will be used for all traces.
   --  Call not thread safe.

   procedure Enable_Section (Section : in String; Enabled : in Boolean := True);
   --  Apply to all tracers registered.

   procedure Enable_Section (Section : String;
                             Level   : All_Levels);

   procedure Set_Decorator (Decor : in Decorator);
   --  Apply to all tracers registered.

   procedure Set_Level (Level : in All_Levels);
   --  Apply to all tracers registered.

   function Console_Tracer return Object_Access;
   --  Reference to the default tracer.

   --  Exception helpers:

   function External_Tag (Tag : in Ada.Tags.Tag) return String renames
     Ada.Tags.External_Tag;

   function Report (E : Ada.Exceptions.Exception_Occurrence) return String;

   type String_Access is access all String;

   type Logger (Text  : String_Access;
                Level : Levels) is new Ada.Finalization.Limited_Controlled
   with null record;
   --  Used to log inline in declarative blocks...

private

   overriding
   procedure Initialize (This : in out Logger);

   --  C helpers
   procedure C_Log (Text    : Interfaces.C.Strings.Chars_Ptr;
                    Level   : Interfaces.C.Int;
                    Section : Interfaces.C.Strings.Chars_Ptr);
   pragma Export (C, C_Log, "agpl__trace__log");

   type Object is abstract tagged limited null record;

end Agpl.Trace;
