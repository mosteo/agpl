private with Ada.Finalization;

with Agpl.Types;

with Png_Io;

package Agpl.Png is

   --  Add facilities for normalized (0 -- 255) values
   --  Ultra, slow, ultra dumb, don't work with ALPHA images!!
   pragma Piece_Of_Shit;

   Log_Section : constant String := "agpl.png";

   type Png_File is tagged limited private;

   subtype Coordinate is Png_Io.Coordinate;
   subtype Dimension  is Png_Io.Dimension;

   procedure Open  (F : in out Png_File; Filename : in String);
   --  No need, to close, it's controlled

   function Handle (This : Png_File) return access Png_Io.Png_File;
   --  Use this to get access to the rest of Png_Io facilities... yes...
   --     I could wrap everything but I don't care.

   function  Width (F : Png_File) return Dimension;
   function Height (F : Png_File) return Dimension;

   function   Red_Value (F : Png_File; R, C : Coordinate) return Agpl.Types.Unsigned_8;
   function Green_Value (F : Png_File; R, C : Coordinate) return Agpl.Types.Unsigned_8;
   function  Blue_Value (F : Png_File; R, C : Coordinate) return Agpl.Types.Unsigned_8;
   --  These three return a value in 0 -- 255
   --  For monochrome PNGs, the three return the same value.

   function   Avg_Value (F : Png_File; R, C : Coordinate) return Agpl.Types.Unsigned_8;

private

   type Png_File is new Ada.Finalization.Limited_Controlled with record
      Png : aliased Png_Io.Png_File;

      Is_Open : Boolean := False;

      Norm    : Float;
      Coltype : Png_Io.Colour_Type_Code;
   end record;

   procedure Finalize (This : in out Png_File);

   function N (This : Png_File; X : Integer) return Agpl.Types.Unsigned_8;
   --  Normalize

end Agpl.Png;
