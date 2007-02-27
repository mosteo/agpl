------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

--  Utilities for PostgreSQL databases

with Gnu.Db.Postgresql; use Gnu.Db.Postgresql;

with Ada.Finalization;

package Agpl.Db.Psql is

   pragma Preelaborate;

   type Database_Access is access all Database;

   function Connect (Db   : String;
                     User : String;
                     Pass : String;
                     Host : String  := "127.0.0.1";
                     Port : Natural := 5432;
                     Ssl  : Boolean := True) return Database_Access;
   --  Connection status is not checked; caller must check it.

   procedure Check_Query (R : Result; Db : Database);
   --  Asserts that the query hasn't returned error
   --  Also issues a rollback

   procedure Query (R : out Result; Db : Database; Q : String);
   --  Performs the query and checks for failure

   function Query (Db : Database; Sql : String) return String;
   --  Get the first column of the first row of a query.
   --  Useful for count and the like

   procedure Begin_Transaction (Db : Database);

   procedure Commit_Transaction (Db : Database);
   procedure Commit (Db : Database) renames Commit_Transaction;

   procedure Rollback_Transaction (Db : Database);
   procedure Rollback (Db : Database) renames Rollback_Transaction;

   type Transaction (Db : access Database) is limited private;
   --  Controlled type that will issue a Begin on creation and a Commit on
   --  destruction.

   function Escape (This : String) return String;
   function Unescape (This : String) return String;
   --  The ' character!

private

   type Transaction (Db : access Database) is
     new Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (This : in out Transaction);
   procedure Finalize   (This : in out Transaction);

end Agpl.Db.Psql;
