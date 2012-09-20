

with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;

with Agpl.Conversions;
with Agpl.Strings;

with Ada.Streams.Stream_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

with Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;

package body Agpl.Http.Server.Sort_handler is

   use type UString;

   --  Settings in disk here:
   Settings_file : UString := Null_Ustring;

   pragma UhOh ("I'm pretty sure the next should be a multimap which doesn't exist in the standard lib but did in Charles");
   package Orderers is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Natural);

   type Vector_tag_array is array (Positive range <>) of
      Templates_Parser.Vector_Tag;

   ------------------------------------------------------------------------
   -- Caching                                                            --
   ------------------------------------------------------------------------
   --  We cache the settings used for sort/sense in each page
   package Orders_vectors is new Agpl.Containers.Naked_Vectors (Positive);
   Max_orders : constant Positive := 2;
   type Page_settings is record
      Columns   : Orders_vectors.Object (First => 1);
      Ascending : Boolean     := True;
      Lines     : Positive    := Default_Lines_Per_Page;
      Page      : Positive    := 1;
   end record;

   package Settings_caches is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Page_Settings);

   Settings_cache : Settings_caches.Map;

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
      Request : in AWS.Status.Data) return Page_settings
   is
      Params   : Constant Aws.Parameters.List := AWS.Status.Parameters (Request);
      Column   : Natural;
      Order    : Boolean;
      Settings : Page_settings;
      Defaults : Page_settings;
      use Settings_caches;
      use Orders_vectors;
   begin
      Append (Defaults.Columns, 1);

      if Contains (Settings_Cache, This.Page.all) then
         Settings := Element (Find (Settings_cache, This.Page.all));
      else
         Settings := Defaults;
      end if;

      --  Obtain ordering index:
      begin
         Column := Natural'Value (AWS.Parameters.Get (Params, "orden"));
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

      --  Obtain way of ordering:
      begin
         Order := Boolean'Value (AWS.Parameters.Get (Params, "sentido"));
         Settings.Ascending := Order;
      exception
         when others =>
            null; -- From 'Value
      end;

      --  Page
      begin
         Settings.Page := Positive'Value (AWS.Parameters.Get (Params, "page"));
      exception
         when others =>
            null; -- From 'Value
      end;

      --  Lines per page
      --  Not cached or saved, if missing we use the default for the object.
      begin
         Settings.Lines := Positive'Value (AWS.Parameters.Get (Params, "lines"));
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
      Request : in AWS.Status.Data) return AWS.Response.Data
   is
      Data     : Data_set;
      Cont     : Orderers.Map;
      Settings : Page_settings := Get_settings (This, Request);

      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Datasets;
      use Orderers;
      use Templates_Parser;
      use Settings_caches;
   begin
      --  Store new settings:
      Delete (Settings_cache, This.Page.all);
      Insert (Settings_cache, This.Page.all, Settings);

      --  Obtain data:
      This.Source (Data);

      --  Dummy response if no data:
      if Last (Data) = 0 then
         return -- <-------------------- EARLY EXIT!!
            AWS.Response.Build (
               AWS.MIME.Content_Type (This.Page.all),
               UString'(Parse (
                  Get_Root & This.Page.all,
                  Standard_Xlats (Request) & This.Single.all,
                  Cached => False)),
               Cache_Control => AWS.Messages.No_Cache);
      end if;

      --  Insert it in the container:
      for N in 1 .. Last (Data) loop
         declare
            Indexer : UString := Null_Ustring;
            Sep     : constant UString := U (":");
            use Orders_vectors;
         begin
            for M in 1 .. Last (Settings.Columns) loop
               Indexer := Indexer & Sep & Data.Vector (N).Vector (
                     Settings.Columns.Vector (M)).Order_value;
            end loop;
            Insert (Cont, S (Indexer), N);
         end;
      end loop;

      --  Create filters
      declare
         Values : Vector_tag_array (1 .. Last (Data.Vector (1)));
         I      : Orderers.Cursor;
         Transl : Translate_Table (Values'First .. Values'Last);
         Extras : Translate_Table := (
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
               Cont.Replace_Element (I, Element (I) + (Settings.Page - 1) * Settings.Lines);
            exception
               when others =>
                  I := First (Cont);
            end;
         else
            I := Last (Cont);
            begin
               Cont.Replace_Element (I, Element (I) - (Settings.Page - 1) * Settings.Lines);
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
               I := Next (I);
            else
               I := Previous (I);
            end if;
         end loop;
         for N in Transl'Range loop
            Transl (N) := Assoc ("VALUE" & Trim (N'Img, Both), Values (N));
         end loop;

         --  Create pager
         declare
            Tag : UString;
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
         return AWS.Response.Build (
            AWS.MIME.Content_Type (This.Page.all),
            UString'(Parse (
               Get_Root & This.Page.all,
               Transl & Extras & Standard_Xlats (Request) & This.Single.all & Pager,
             Cached => False)),
            Cache_Control => AWS.Messages.No_Cache);
      end;
   end Get_page;

   ------------------------------------------------------------------------
   -- Set_Lines_Per_Page                                                 --
   ------------------------------------------------------------------------
   --  To change the default number of records per page.
   procedure Set_Lines_Per_Page (This : in out Object; Lines : in Positive) is
   begin
      This.Lines_Per_Page := Lines;
   end Set_Lines_Per_Page;

   ------------------------------------------------------------------------
   -- Set_settings_file                                                  --
   ------------------------------------------------------------------------
   --  To indicate where (path + name) to save ordering prefs.
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
      use Ada.Streams.Stream_IO;
      F : File_Type;
   begin
      if not GNAT.OS_Lib.Is_Regular_File (To_String (Settings_file)) then
         return;
      end if;

      Open (F, Name => To_String (Settings_file), Mode => In_File);
      while not End_Of_File (F) loop
         declare
            S : Page_settings;
            K : UString;
         begin
            K := UString'Input (Stream (F));
            Page_settings'Read (Stream (F), S);
            Insert (Settings_cache, To_String (K), S);
         end;
      end loop;
      Close (F);
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
            raise;
         end if;
   end Load_settings;
   ------------------------------------------------------------------------
   -- Save_settings                                                      --
   ------------------------------------------------------------------------
   procedure Save_settings is
      use Settings_caches;
      use Ada.Streams.Stream_IO;
      F : File_Type;
      I : Cursor := First (Settings_cache);
   begin
      if Settings_file = Null_Ustring then
         return;
      end if;
      Create (F, Name => To_String (Settings_file), Mode => Out_File);
      while Has_Element (I) loop
         UString'Output (Stream (F), To_Ustring (Key (I)));
         Page_settings'Write (Stream (F), Element (I));
         I := Next (I);
      end loop;
      Close (F);
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
            raise;
         end if;
   end Save_settings;

   ------------------------------------------------------------------------
   -- Void_singleton                                                     --
   ------------------------------------------------------------------------
   --  Dummy auxiliary singleton_function which returns the empty translation.
   function Void_singleton return Templates_Parser.Translate_Table is
   begin
      return Templates_Parser.No_Translation;
   end Void_singleton;

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   --  Auxiliary to ease creation of sorting fields based in integers
   function Rpad (I : in Integer; Size : in Natural := 11) return UString is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      V : constant String :=
         String'(1 .. Size => '0') & Trim (Integer'Image (I), Both);
   begin
      if I < 0 then
         return Rpad (Integer'Last + I, Size);
      else
         return U (V (V'Last - Size + 1 .. V'Last));
      end if;
   end Rpad;

   function Rpad (I : in Float; Size : in Natural := 11) return UString is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      V : constant String :=
         String'(1 .. Size => '0') & Agpl.Strings.To_string (I);
   begin
      if I < 0.0 then
         return Rpad (Float'Last + I, Size);
      else
         return U (V (V'Last - Size + 1 .. V'Last));
      end if;
   end Rpad;

   function Rpad (I : in Duration; Size : in Natural := 11) return UString is
   begin
      return Rpad (Float (I), Size);
   end Rpad;

end Agpl.Http.Server.Sort_handler;
