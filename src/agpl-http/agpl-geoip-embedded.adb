 

--  Uses the CSV database from www.maxmind.com

--  Allows initialization from an embedded database, if present.

with Aws.Resources;

package body Agpl.Geoip.Embedded is

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Call it before any other subroutine.
   -- Database is the name of the embedded resource (using Awsres).
   procedure Init (Database : in String; Success : out Boolean) is
      use Aws.Resources;
      F    : File_type;
      Line : String (1 .. 255);
      Last : Natural;
   begin
      Success := false; -- By now

      if not Aws.Resources.Is_regular_file (Database) then
         Success := false;
         return;  --  <-- Early exit, file not found.
      end if;

      Open (F, Database);
      while not End_of_file (F) loop
         Get_line (F, Line, Last);
         Parse_line (Line (Line'First .. Last));
      end loop;
      Close (F);

      Success := true;
   exception
      when others =>
         Close (F);
         raise;
   end Init;

end Agpl.Geoip.Embedded;
