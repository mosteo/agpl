

--  Objects for tracing with disc dump optional.
--  This first implementation uses a protected object but not queuing.
--  Writing is done within the protected, which is theoretically illega.
--  Gnat's implementation of I/O allows it so in this first approach we'll
--  leave it like that.

with Agpl.Debug;
with Agpl.Trace.Console;

with Ada.Containers.Vectors;

package body Agpl.Trace is

   package Tracer_Vectors is new Ada.Containers.Vectors
     (Positive, Object_Access);

   Stdout  : aliased Console.Object;

   Tracers : Tracer_Vectors.Vector;

   -- Safe logger --

   protected Safe is
      procedure Log (This    : in out Object'Class;
                     Text    : in     String;
                     Level   : in     Levels;
                     Section : in String := "");
   end Safe;

   protected body Safe is
      procedure Log (This    : in out Object'Class;
                     Text    : in     String;
                     Level   : in     Levels;
                     Section : in String := "")
      is
      begin
         This.Log (Text, Level, Section); -- Dispatch to the logger object.
      end Log;
   end Safe;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Logger) is
   begin
      Log (This.Text.all, This.Level);
   end Initialize;

   ----------------
   -- Add_Tracer --
   ----------------

   procedure Add_Tracer (This : not null Object_Access) is
   begin
      --  We defer to post-elaboration the addition of the default tracer
      if Tracers.Is_Empty and then This /= Stdout'Access then
         Tracers.Append (Stdout'Access);
      end if;
      Tracers.Append (This);
   end Add_Tracer;

   --------------------
   -- Console_Tracer --
   --------------------

   function Console_Tracer return Object_Access is
   begin
      return Stdout'Access;
   end Console_Tracer;

   --------------------
   -- Enable_Section --
   --------------------

   procedure Enable_Section (Section : in String; Enabled : in Boolean := True)
   is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Enable_Section (Section, Enabled);
      end Do_It;
   begin
      if Tracers.Is_Empty  then
         Tracers.Append (Stdout'Access);
      end if;
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Enable_Section;

   --------------------
   -- Enable_Section --
   --------------------

   procedure Enable_Section (Section : String;
                             Level   : All_Levels)
   is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Enable_Section (Section, Level);
      end Do_It;
   begin
      if Tracers.Is_Empty  then
         Tracers.Append (Stdout'Access);
      end if;
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Enable_Section;

   ---------
   -- Log --
   ---------
   --  In purpose, This can be null to allow the passing of Null_Object.
   procedure Log
     (This    : in     Object_Access;
      Text    : in     String;
      Level   : in     Levels;
      Section : in String := "") is
   begin
      if Enabled then
         if This /= null and then This.Must_Log (Level, Section) then
            Safe.Log (This.all, Text, Level, Section);
         end if;
      end if;
   end Log;


   ---------
   -- Log --
   ---------
   --  Logs to the default log object.
   procedure Log
     (Text    : in String;
      Level   : in Levels;
      Section : in String := "") is
   begin
      if Enabled then
         --  We defer to post-elaboration the addition of the default tracer
         if Tracers.Is_Empty then
            Tracers.Append (Stdout'Access);
         end if;

         for I in Tracers.First_Index .. Tracers.Last_Index loop
            if Tracers.Element (I).Must_Log (Level, Section) then
               Safe.Log (Tracers.Element (I).all, Text, Level, Section);
            end if;
         end loop;
      end if;
   end Log;

   ---------------
   -- Would_Log --
   ---------------

   function Would_Log
     (Level   : in Levels;
      Section : in String := "") return Boolean is
   begin
      if Enabled then
         --  We defer to post-elaboration the addition of the default tracer
         if Tracers.Is_Empty then
            Tracers.Append (Stdout'Access);
         end if;

         for I in Tracers.First_Index .. Tracers.Last_Index loop
            if Tracers.Element (I).Must_Log (Level, Section) then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Would_Log;

   -------------------
   -- Set_Decorator --
   -------------------

   procedure Set_Decorator (Decor : in Decorator) is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Set_Decorator (Decor);
      end Do_It;
   begin
      if Tracers.Is_Empty  then
         Tracers.Append (Stdout'Access);
      end if;
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Set_Decorator;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (Level : in All_Levels) is
      procedure Do_It (X : in out Object_Access) is
      begin
         X.Set_Level (Level);
      end Do_It;
   begin
      if Tracers.Is_Empty  then
         Tracers.Append (Stdout'Access);
      end if;
      for I in Tracers.First_Index .. Tracers.Last_Index loop
         Tracers.Update_Element (I, Do_It'Access);
      end loop;
   end Set_Level;

   -----------
   -- C_Log --
   -----------

   procedure C_Log (Text    : Interfaces.C.Strings.Chars_Ptr;
                    Level   : Interfaces.C.Int;
                    Section : Interfaces.C.Strings.Chars_Ptr)
   is
      use Interfaces.C.Strings;
   begin
      Log (Value (Text), Levels'Val (Level), Value (Section));
   end C_Log;

   ------------
   -- Report --
   ------------

   function Report (E : Ada.Exceptions.Exception_Occurrence) return String is
   begin
      return Agpl.Debug.Report (E);
   end Report;

end Agpl.Trace;
