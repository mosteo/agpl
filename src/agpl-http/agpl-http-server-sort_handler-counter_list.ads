 

--  Keep track of items by key.

with Templates_parser;

generic
   Show_Negatives : Boolean := False;
package Agpl.Http.Server.Sort_Handler.Counter_List is

   pragma Elaborate_Body;

   No_More_Keys : exception;

   ------------------------------------------------------------------------
   -- Sum_key                                                            --
   ------------------------------------------------------------------------
   -- Add or remove to a key
   -- Use Inc => -1 to remove.
   procedure Sum_Key (Key : in String; Inc : in Integer := 1);

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   -- Elements under a given key
   function Count (Key : in String) return Integer;

   ------------------------------------------------------------------------
   -- Get_First_Key                                                      --
   ------------------------------------------------------------------------
   -- May raise No_More_Keys
   function Get_First_Key return String;

   ------------------------------------------------------------------------
   -- Get_Next_Key                                                       --
   ------------------------------------------------------------------------
   -- May raise No_More_Keys
   function Get_Next_Key (Current : in String) return String;

   -------------
   -- Iterate --
   -------------
   type Iterate_Code is access procedure (Key   : in String;
                                          Count : in Integer);

   procedure Iterate
     (Process   :    Iterate_Code;
      Negatives : in Boolean := False);
   -- Process will be called for each key/count pair

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Generates the data for the html report.
   -- Key, Count, %, Rounded %, Rounded, rounded (n / max)
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------------------------------------------------------------------
   -- Total_Count                                                        --
   ------------------------------------------------------------------------
   -- Sum of all key values
   function Total_Count return Integer;

   ------------------------------------------------------------------------
   -- Total_Report                                                       --
   ------------------------------------------------------------------------
   -- SINGLE1 <-- Sum of all key values
   function Total_Report return Templates_parser.Translate_table;

end Agpl.Http.Server.Sort_Handler.Counter_List;
