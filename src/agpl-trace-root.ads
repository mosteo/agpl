
--  Just implements sections but still doesn't trace.
with Ada.Containers.Indefinite_Ordered_Maps;
with Agpl.Containers.String_Sets;

package Agpl.Trace.Root is

   pragma Preelaborate;

   Bypass_Section_Level : Levels := Warning;
   --  >= this is logged regardless of section

   type Object is limited new Trace.Object with private;
   type Object_Access is access all Object'Class;

   pragma Preelaborable_Initialization (Object);

   overriding
   procedure Log (This    : in out Object;
                  Text    : in     String;
                  Level   : in     Levels;
                  Section : in     String := "") is null;
   --  No need for implementations of this object to call Must_Log, it's called
   --  in the Agpl.Trace.Log subprogram.

   overriding
   function Must_Log (This    : in Object;
                      Level   : in Levels;
                      Section : in String) return Boolean;

   overriding
   procedure Enable_Section  (This    : in out Object;
                              Section : in     String;
                              Enabled : in     Boolean := True);

   overriding
   procedure Enable_Section (This    : in out Object;
                             Section : String;
                             Level   : All_Levels);

   overriding
   procedure Set_Active (This : in out Object; Active : in Boolean := True);

   overriding
   procedure Set_Level  (This : in out Object; Level : in All_Levels);

   overriding
   procedure Set_Decorator (This : in out Object; Decor : in Decorator);

   overriding
   function Decorate (This    : in Object;
                      Text    : in String;
                      Level   : in Levels;
                      Section : in String) return String;

private

   package Section_Level_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, All_Levels);

   type Object is limited new Trace.Object with record
      Active   : Boolean    := True;
      Level    : All_Levels := Informative;
      Sections : Containers.String_Sets.Set;
      Section_Levels : Section_Level_Maps.Map;
      Decor    : Decorator;
   end record;

end Agpl.Trace.Root;
