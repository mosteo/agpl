 

--  Uses the CSV database from www.maxmind.com

--  Allows initialization from an embedded database, if present.

package Agpl.Geoip.Embedded is

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Call it before any other subroutine.
   -- Database is the name of the embedded resource (using Awsres).
   procedure Init (Database : in String; Success : out Boolean);

end Agpl.Geoip.Embedded;
