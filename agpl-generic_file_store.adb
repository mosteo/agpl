with Ada.Streams.Stream_Io;
with Ada.Text_Io;

package body Agpl.Generic_File_Store is

   -------------
   -- To_File --
   -------------

   procedure To_File (This : in Item; File : in String) is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Create (F, Name => File, Mode => Out_File);

      Item'Output (Stream (F), This);
      Close (F);
   end To_File;

   ----------
   -- Load --
   ----------

   function Load (File : in String) return Item is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Open (F, Name => File, Mode => In_File);

      Ada.Text_Io.Put_Line ("In load item");
      declare
         Result : constant Item := Item'Input (Stream (F));
      begin
         Close (F);
         return Result;
      end;
   end Load;

   ----------
   -- Load --
   ----------

   function Load (File : in String) return Item_Access is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Open (F, Name => File, Mode => In_File);

      Ada.Text_Io.Put_Line ("In load access");
      declare
         Result : constant Item_Access := new Item'(Item'Input (Stream (F)));
      begin
         Close (F);
         return Result;
      end;
   end Load;

end Agpl.Generic_File_Store;
