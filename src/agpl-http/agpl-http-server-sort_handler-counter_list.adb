

--  Keep track of hubs in each country:

with Agpl.Aux.Indexer_String_Integer;
with Agpl.Strings;


package body Agpl.Http.Server.Sort_Handler.Counter_List is

   package Indexer renames Agpl.Aux.Indexer_String_Integer;

   ------------------------------------------------------------------------
   -- Counter                                                            --
   ------------------------------------------------------------------------
   protected Counter is
      procedure Add (Key : String; Amount : in Integer := 1);
      function  Get (Key : String) return Integer;
      function  Get_First_Key return String;
      function  Get_Next_Key (Key : String) return String;
      procedure Iterate
        (Process :   Iterate_Code;
         Negatives : in Boolean := False);
      procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);
      function  Total_Count return Integer;
   private
      Total : Integer := 0;
      Table : Indexer.Map;
   end Counter;
   protected body Counter is
      ---------
      -- Add --
      ---------
      procedure Add (Key : String; Amount : in Integer := 1) is
         use Indexer;
         I         : Cursor := Find (Table, Key);
         New_value : Integer;
      begin
         Total := Total + Amount;

         if Has_Element (I) then
            New_value := Element (I) + Amount;
            Delete (Table, I);
            if New_value > 0 then
               Insert (Table, Key, New_value);
            end if;
         else
            Insert (Table, Key, Amount);
         end if;
      end Add;

      ---------
      -- Get --
      ---------
      function  Get (Key : String) return Integer is
         use Indexer;
         I : constant Cursor := Find (Table, Key);
      begin
         if Has_Element (I) then
            return Element (I);
         else
            return 0;
         end if;
      end Get;

      -------------------
      -- Get_First_Key --
      -------------------
      function  Get_First_Key return String is
         use Indexer;
      begin
         if Is_Empty (Table) then
            raise No_More_Keys;
         else
            return Key (First (Table));
         end if;
      end Get_First_Key;

      ------------------
      -- Get_Next_Key --
      ------------------
      function  Get_Next_Key (Key : String) return String is
         use Indexer;
         I : constant Cursor := Find (Table, Key);
      begin
         if (not Has_Element (I)) or else not Has_Element (Next (I)) then
            raise No_More_Keys;
         else
            return Indexer.Key (Next (I));
         end if;
      end Get_Next_Key;

      -------------
      -- Iterate --
      -------------
      procedure Iterate
        (Process :   Iterate_Code;
         Negatives : in Boolean := False)
      is
         use Indexer;
         I : Cursor := First (Table);
      begin
            while Has_Element (I) loop
               if Negatives or else Element (I) >= 0 then
                  Process (Key (I), Element (I));
               end if;
               I := Next (I);
            end loop;
      end Iterate;

      ------------
      -- Report --
      ------------
      procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set) is
         use Agpl.Http.Server.Sort_handler;
         use Indexer;
         I      : Cursor  := First (Table);
         FTotal : constant Float := Float (Total);
         FMax   : Float          := 1.0;
      begin
         -- Rows:
         I := First (Table);
         while Has_Element (I) loop
            if Element (I) > 0 or else Show_Negatives then
               declare
                  Row : Data_row;
               begin
                  -- Key
                  Append (Row, Item => (
                     Value       => U (Key (I)),
                     Order_value => U (Key (I))));
                  -- Value
                  Append (Row, Item => (
                     Value       => U (
                        Agpl.Strings.Trim (Integer'Image (Element (I)))),
                     Order_value => Rpad (Element (I))));
                  -- Percents
                  Append (Row, (
                     Value       => U (Agpl.Strings.Trim (
                        Agpl.Strings.To_string (
                           Float (Element (I)) * 100.0 / FTotal, 1))),
                     Order_value => Rpad (Float (Element (I)) / FTotal)));
                  -- Rounded percents upped to a minimum of 1
                  declare
                     Percent : Float := Float (Element (I)) * 100.0 / FTotal;
                  begin
                     Percent := Float'Max (Percent, 1.0);
                     Append (Row, (
                        Value  => U (Agpl.Strings.To_string (Integer (Percent))),
                        Order_value => Rpad (Integer (Percent))));
                  end;

                  -- ROW
                  Append (Data, Row);

                  Fmax := Float'Max (Fmax, Float (Element (I)));
               end;
            end if;
            I := Next (I);
         end loop;
         -- Create last value getting normalized values across 1 -- 100 (ints)
         begin
            I := First (Table);
            for N in 1 .. Last (Data) loop
               Append (Data.Vector (N), (
                  U (Agpl.Strings.To_string (Integer'Max (1,
                     Integer (
                        Float (Element (I)) * 100.0 / FMax)))),
                  Rpad (Integer (Float (Element (I)) * 100.0 / Fmax))));
               I := Next (I);
            end loop;
         end;
      end Report;

      -----------------
      -- Total_Count --
      -----------------
      function Total_Count return Integer is
      begin
         return Total;
      end Total_Count;
   end Counter;

   ------------------------------------------------------------------------
   -- Sum_Key                                                            --
   ------------------------------------------------------------------------
   procedure Sum_Key (Key : in String; Inc : in Integer := 1) is
   begin
      Counter.Add (Key, Inc);
   end Sum_Key;

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   function Count (Key : in String) return Integer is
   begin
      return Counter.Get (Key);
   end Count;

   ------------------------------------------------------------------------
   -- Get_First_Key                                                      --
   ------------------------------------------------------------------------
   -- May raise No_More_Keys
   function Get_First_Key return String is
   begin
      return Counter.Get_First_Key;
   end Get_First_Key;

   ------------------------------------------------------------------------
   -- Get_Next_Key                                                       --
   ------------------------------------------------------------------------
   -- May raise No_More_Keys
   function Get_Next_Key (Current : in String) return String is
   begin
      return Counter.Get_Next_Key (Current);
   end Get_Next_Key;

      -------------
      -- Iterate --
      -------------
      procedure Iterate
        (Process   : Iterate_Code;
         Negatives : in Boolean := False)
      is
      begin
         Counter.Iterate (Process, Negatives);
      end Iterate;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Generates the data for the html report.
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set) is
   begin
      Counter.Report (Data);
   end Report;

   ------------------------------------------------------------------------
   -- Total_Count                                                        --
   ------------------------------------------------------------------------
   function Total_Count return Integer is
   begin
      return Counter.Total_Count;
   end Total_Count;

   ------------------------------------------------------------------------
   -- Total_Report                                                       --
   ------------------------------------------------------------------------
   -- SINGLE1 <-- total
   function Total_Report return Templates_parser.Translate_table is
      use Templates_parser;
   begin
      return (1 => Assoc ("SINGLE1", Counter.Total_Count));
   end Total_Report;

end Agpl.Http.Server.Sort_Handler.Counter_List;
