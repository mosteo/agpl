

with Aws.Messages;
with Aws.Mime;
with Aws.Parameters;
with Aws.Response;
with Aws.Status;
with Templates_parser;

with Agpl.Conversions;
with Agpl.Dynamic_vector;
with Agpl.Strings;

with Charles.Maps.Sorted.Strings.Unbounded;
with Charles.Multimaps.Sorted.Strings.Unbounded;

with Ada.Streams.Stream_io;
with Ada.Strings;
with Ada.Strings.Fixed;

with Gnat.Os_lib;

with Text_Io;

package body Agpl.Http.Server.Sort_handler is

   use type Ustring;

   -- Settings in disk here:
   Settings_file : Ustring := Null_ustring;

   package Orderers is new Charles.Multimaps.Sorted.Strings.Unbounded (
      Natural, "<", "=");

   type Vector_tag_array is array (Positive range <>) of
      Templates_parser.Vector_tag;

   ------------------------------------------------------------------------
   -- Caching                                                            --
   ------------------------------------------------------------------------
   -- We cache the settings used for sort/sense in each page
   package Orders_vectors is new Agpl.Dynamic_vector (Positive);
   Max_orders : constant Positive := 2;
   type Page_settings is record
      Columns   : Orders_vectors.Object (First => 1);
      Ascending : Boolean     := true;
      Lines     : Positive    := Default_Lines_Per_Page;
      Page      : Positive    := 1;
   end record;

   package Settings_caches is new Charles.Maps.Sorted.Strings.Unbounded (
      Page_settings, "<", "=");

   Settings_cache : Settings_caches.Container_type;

   procedure Load_settings;
   procedure Save_settings;

   procedure Put_settings (Settings : Page_Settings) is
      use Orders_Vectors;
   begin
      if Length (Settings.Columns) > 0 then
         Text_IO.Put_Line ("SORTCOLUMN" & Natural'Image (Settings.Columns.Vector (1)));
      else
         Text_IO.Put_Line ("SORTCOLUMN VOID");
      end if;
   end Put_Settings;

   function Get_settings (
      This    : in Object;
      Request : in Aws.Status.Data) return Page_settings
   is
      Params   : Aws.Parameters.List := Aws.Status.Parameters (Request);
      Column   : Natural;
      Order    : Boolean;
      Settings : Page_settings;
      Defaults : Page_settings;
      use Settings_caches;
      use Orders_vectors;
   begin
      Append (Defaults.Columns, 1);

      if Is_in (This.Page.all, Settings_cache) then
         Settings := Element (Find (Settings_cache, This.Page.all));
      else
         Settings := Defaults;
      end if;

      -- Obtain ordering index:
      begin
         Column := Natural'Value (Aws.Parameters.Get (Params, "orden"));
         if Length (Settings.Columns) = 0 or else
            Settings.Columns.Vector (1) /= Column
         then
            Insert (Settings.Columns, Column, 1);
            if Length (Settings.Columns) > Max_orders then
               Delete (Settings.Columns, Last (Settings.Columns));
            end if;
         end if;
      exception
         when others =>
            null; -- From 'Value
      end;

      -- Obtain way of ordering:
      begin
         Order := Boolean'Value (Aws.Parameters.Get (Params, "sentido"));
         Settings.Ascending := Order;
      exception
         when others =>
            null; -- From 'Value
      end;

      -- Page
      begin
         Settings.Page := Positive'Value (Aws.Parameters.Get (Params, "page"));
      exception
         when others =>
            null; -- From 'Value
      end;

      -- Lines per page
      -- Not cached or saved, if missing we use the default for the object.
      begin
         Settings.Lines := Positive'Value (Aws.Parameters.Get (Params, "lines"));
      exception
         when others =>
            Settings.Lines := This.Lines_Per_Page;
      end;

      return Settings;
   end Get_settings;

   ------------------------------------------------------------------------
   -- Get_page                                                           --
   ------------------------------------------------------------------------
   function Get_page (
      This    : in Object;
      Request : in Aws.Status.Data) return Aws.Response.Data
   is
      Data     : Data_set;
      Cont     : Orderers.Container_type;
      Settings : Page_settings := Get_settings (This, Request);

      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Datasets;
      use Orderers;
      use Templates_parser;
      use Settings_caches;
   begin
      -- Store new settings:
      Delete (Settings_cache, This.Page.all);
      Insert (Settings_cache, This.Page.all, Settings);

      -- Obtain data:
      This.Source (Data);

      -- Dummy response if no data:
      if Last (Data) = 0 then
         return -- <-------------------- EARLY EXIT!!
            Aws.Response.Build (
               Aws.Mime.Content_type (This.Page.all),
               UString'(Parse (
                  Get_root & This.Page.all,
                  Standard_xlats (Request) & This.Single.all,
                  Cached => false)),
               Cache_control => Aws.Messages.No_cache);
      end if;

      -- Insert it in the container:
      for N in 1 .. Last (Data) loop
         declare
            Indexer : Ustring := Null_ustring;
            Sep     : constant Ustring := U (":");
            use Orders_vectors;
         begin
            for M in 1 .. Last (Settings.Columns) loop
               Indexer := Indexer & Sep & Data.Vector (N).Vector (
                     Settings.Columns.Vector (M)).Order_value;
            end loop;
            Insert (Cont, S (Indexer), N);
         end;
      end loop;

      -- Create filters
      declare
         Values : Vector_tag_array (1 .. Last (Data.Vector (1)));
         I      : Orderers.Iterator_type;
         Transl : Translate_table (Values'First .. Values'Last);
         Extras : Translate_table := (
            1 => Assoc ("SENTIDO", not Settings.Ascending),
            2 => Assoc ("ORDEN",   Settings.Columns.Vector (1)),
            3 => Assoc ("LINES",   Settings.Lines),
            4 => Assoc ("PAGE",    Settings.Page)
            );
         Pager  : Translate_Table (1 .. 1);
      begin
         if Settings.Ascending then
            I := First (Cont);
            begin
               Increment (I, (Settings.Page - 1) * Settings.Lines);
            exception
               when others =>
                  I := First (Cont);
            end;
         else
            I := Last (Cont);
            begin
               Decrement (I, (Settings.Page - 1) * Settings.Lines);
            exception
               when others =>
                  I := Last (Cont);
            end;
         end if;
         for N in (Settings.Page - 1) * Settings.Lines + 1 ..
                  Natural'Min (Last (Data), Settings.Page * Settings.Lines) loop
--       for N in 1 .. Last (Data) loop
            for M in Values'Range loop
               Values (M) :=
                  Values (M) & Data.Vector (Element (I)).Vector (M).Value;
            end loop;
            if Settings.Ascending then
               I := Succ (I);
            else
               I := Pred (I);
            end if;
         end loop;
         for N in Transl'Range loop
            Transl (N) := Assoc ("VALUE" & Trim (N'Img, Both), Values (N));
         end loop;

         -- Create pager
         declare
            Tag : Ustring;
         begin
            ASU.Append (Tag, "<span class=""pager"">");
            for I in 1 .. (Length (Data) - 1) / Settings.Lines + 1 loop
               if I /= 1 then
                  ASU.Append (Tag, " | ");
               end if;
               if I /= Settings.Page then
                  ASU.Append (Tag,
                     "<a href=""?orden=" & Conversions.To_String (Settings.Columns.Vector (1)) &
                     "&sentido=" & Strings.To_Lower (Boolean'Image (Settings.Ascending)) &
                     "&page=" & Conversions.To_String (I) &
                     "&lines=" & Conversions.To_String (Settings.Lines) &
                     """>Page " & Conversions.To_String (I) & "</a>");
               else
                  ASU.Append (Tag, "<b>Page " & Conversions.To_String (I) & "</b>");
               end if;
            end loop;
            ASU.Append (Tag, "</span>");
            Pager (1) := Assoc ("PAGER", S (Tag));
         end;

         --  Save sorting settings:
         Save_settings;

         --  Return filtered:
         return Aws.Response.Build (
            Aws.Mime.Content_type (This.Page.all),
            UString'(Parse (
               Get_root & This.Page.all,
               Transl & Extras & Standard_xlats (Request) & This.Single.all & Pager,
             Cached => False)),
            Cache_control => Aws.Messages.No_cache);
      end;
   end Get_page;

   ------------------------------------------------------------------------
   -- Set_Lines_Per_Page                                                 --
   ------------------------------------------------------------------------
   -- To change the default number of records per page.
   procedure Set_Lines_Per_Page (This : in out Object; Lines : in Positive) is
   begin
      This.Lines_Per_Page := Lines;
   end Set_Lines_Per_Page;

   ------------------------------------------------------------------------
   -- Set_settings_file                                                  --
   ------------------------------------------------------------------------
   -- To indicate where (path + name) to save ordering prefs.
   procedure Set_settings_file (This : in String) is
   begin
      Settings_file := U (This);
      Load_settings;
   end Set_settings_file;

   ------------------------------------------------------------------------
   -- Load_settings                                                      --
   ------------------------------------------------------------------------
   procedure Load_settings is
      use Settings_caches;
      use Ada.Streams.Stream_io;
      F : File_type;
   begin
      if not Gnat.Os_lib.Is_regular_file (To_string (Settings_file)) then
         return;
      end if;

      Open (F, Name => To_string (Settings_file), Mode => In_file);
      while not End_of_file (F) loop
         declare
            S : Page_settings;
            K : Ustring;
         begin
            K := Ustring'Input (Stream (F));
            Page_settings'Read (Stream (F), S);
            Insert (Settings_cache, To_string (K), S);
         end;
      end loop;
      Close (F);
   exception
      when others =>
         if Is_open (F) then
            Close (F);
            raise;
         end if;
   end Load_settings;
   ------------------------------------------------------------------------
   -- Save_settings                                                      --
   ------------------------------------------------------------------------
   procedure Save_settings is
      use Settings_caches;
      use Ada.Streams.Stream_io;
      F : File_type;
      I : Iterator_type := First (Settings_cache);
   begin
      if Settings_file = Null_Ustring then
         return;
      end if;
      Create (F, Name => To_string (Settings_file), Mode => Out_file);
      while I /= Back (Settings_cache) loop
         Ustring'Output (Stream (F), To_ustring (Key (I)));
         Page_settings'Write (Stream (F), Element (I));
         I := Succ (I);
      end loop;
      Close (F);
   exception
      when others =>
         if Is_open (F) then
            Close (F);
            raise;
         end if;
   end Save_settings;

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   -- Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton return Templates_parser.Translate_table is
   begin
      return Templates_parser.No_translation;
   end Void_singleton;

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   -- Auxiliary to ease creation of sorting fields based in integers
   function Rpad (I : in Integer; Size : in Natural := 11) return Ustring is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      V : constant String :=
         String'(1 .. Size => '0') & Trim (Integer'Image (I), Both);
   begin
      if I < 0 then
         return Rpad (Integer'Last + I, Size);
      else
         return U (V (V'last - Size + 1 .. V'Last));
      end if;
   end Rpad;

   function Rpad (I : in Float; Size : in Natural := 11) return Ustring is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      V : constant String :=
         String'(1 .. Size => '0') & Agpl.Strings.To_string(I);
   begin
      if I < 0.0 then
         return Rpad (Float'Last + I, Size);
      else
         return U (V (V'last - Size + 1 .. V'Last));
      end if;
   end Rpad;

   function Rpad (I : in Duration; Size : in Natural := 11) return Ustring is
   begin
      return RPad (Float (I), Size);
   end RPad;

end Agpl.Http.Server.Sort_handler;
