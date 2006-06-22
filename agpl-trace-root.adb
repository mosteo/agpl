package body Agpl.Trace.Root is

   ------------
   -- Object --
   ------------

   protected body Object is

      function Must_Log (Level : in Levels; Section : in String) return Boolean
      is
      begin
         return
           Active and then
           Level >= Object.Level and then
           (Level >= Error or else
            Section = "" or else
            Sections.Contains (Section));
      end Must_Log;

      ---------------------
      -- Disable_Section --
      ---------------------

      procedure Disable_Section (Section : String) is
      begin
         Sections.Exclude (Section);
      end Disable_Section;

      --------------------
      -- Enable_Section --
      --------------------

      procedure Enable_Section (Section : String) is
      begin
         Sections.Include (Section);
      end Enable_Section;

      ----------------
      -- Set_Active --
      ----------------

      procedure Set_Active (Active : in Boolean := True) is
      begin
         Object.Active := Active;
      end Set_Active;

      ---------------
      -- Set_Level --
      ---------------

      procedure Set_Level (Level : in All_Levels) is
      begin
         Object.Level := Level;
      end Set_Level;

   end Object;

end Agpl.Trace.Root;
