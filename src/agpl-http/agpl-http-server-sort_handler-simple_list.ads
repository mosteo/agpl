generic
   type Object is private;        -- Storage object containing stats.
   with procedure Generate_row (
      This   : in  Object;
      Is_new : in  Boolean;
      Row    : out Agpl.Http.Server.Sort_handler.Data_row);
   Max_entries : Positive := 999; -- Max stored entries.
package Agpl.Http.Server.Sort_handler.Simple_list is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object);

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear;
   -- Deletes all data

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   -- Creates the listing report
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------------------------------------------------------------------
   -- New_events                                                         --
   ------------------------------------------------------------------------
   -- Says how many new entries are there since last check.
   function New_events return Natural;

end Agpl.Http.Server.Sort_handler.Simple_list;
