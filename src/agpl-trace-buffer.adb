package body Agpl.Trace.Buffer is

   protected body Safe is

      ---------
      -- Add --
      ---------

      procedure Add (X : Log_Entry) is
      begin
         Entries.Append (X);

         while Natural (Entries.Length) > Max_Length loop
            Entries.Delete_First;
         end loop;
      end Add;

      ---------
      -- Get --
      ---------

      function Get return Entry_Lists.List is
      begin
         return Entries;
      end Get;

   end Safe;

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
      This.Entries.Add
        ((Level => Level,
          Text  => +Text,
          Sect  => +Section,
          Stamp => Ada.Calendar.Clock));
   end Log;

   ---------
   -- Get --
   ---------

   function Get
     (This : Object)
      return Entry_Lists.List
   is
   begin
      return This.Entries.Get;
   end Get;

end Agpl.Trace.Buffer;
