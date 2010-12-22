with Ada.Numerics.Elementary_Functions;

with Agpl.Trace; use Agpl.Trace;

package body Agpl.Png is

   package Pio renames Standard.Png_Io;

   ----------
   -- Open --
   ----------

   procedure Open (F : in out Png_File; Filename : in String) is
      use Ada.Numerics.Elementary_Functions;
   begin
      PIo.Open (F.Png, Filename);
      F.Is_Open := True;
      F.Coltype := Pio.Colour_Type (F.Png);

      if Png_Io.Alpha (F.Coltype) then
         raise Program_Error with "PNG with alpha not supported";
      elsif Png_Io.Palette (F.Png) then
         raise Program_Error with "PNG with palette not supported";
      end if;

      Log ("Colour type:" & Pio.Colour_Type (F.Png)'Img,
           Informative, Log_Section);
      Log ("Png dims:" & F.Width'Img & " x" & F.Height'Img,
           Informative, Log_Section);
      Log ("Sample depth:" & Pio.Sample_Depth (F.Png)'Img,
           Informative, Log_Section);

      F.Norm := 2.0 ** Float (Pio.Sample_Depth (F.Png) - 8);

      Log ("Norm:" & F.Norm'Img, Informative, Log_Section);
   end Open;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Png_File) is
   begin
      if This.Is_Open then
         This.Is_Open := False;
         Pio.Close (This.Png);
      end if;
   end Finalize;

   ------------
   -- Handle --
   ------------

   function Handle (This : Png_File) return access Png_Io.Png_File is
   begin
      return This.Png'Unrestricted_Access;
   end Handle;

   ---------------
   -- Red_Value --
   ---------------

   function Red_Value
     (F : Png_File;
      R, C : Coordinate)
      return Agpl.Types.Unsigned_8
   is
   begin
      case F.Coltype is
         when Pio.Zero | Pio.Four =>
            return N (F, Pio.Pixel_Value (F.Png, R, C));
         when Pio.Two | Pio.Three | Pio.Six =>
            return N (F, Pio.Red_Value (F.Png, R, C));
      end case;
   end Red_Value;

   -----------------
   -- Green_Value --
   -----------------

   function Green_Value
     (F : Png_File;
      R, C : Coordinate)
      return Agpl.Types.Unsigned_8
   is
   begin
      case F.Coltype is
         when Pio.Zero | Pio.Four =>
            return N (F, Pio.Pixel_Value (F.Png, R, C));
         when Pio.Two | Pio.Three | Pio.Six =>
            return N (F, Pio.Green_Value (F.Png, R, C));
      end case;
   end Green_Value;

   ----------------
   -- Blue_Value --
   ----------------

   function Blue_Value
     (F : Png_File;
      R, C : Coordinate)
      return Agpl.Types.Unsigned_8
   is
   begin
      case F.Coltype is
         when Pio.Zero | Pio.Four =>
            return N (F, Pio.Pixel_Value (F.Png, R, C));
         when Pio.Two | Pio.Three | Pio.Six =>
            return N (F, Pio.Blue_Value (F.Png, R, C));
      end case;
   end Blue_Value;

   ---------------
   -- Avg_Value --
   ---------------

   function Avg_Value (F : Png_File; R, C : Coordinate)
                       return Agpl.Types.Unsigned_8
   is
      use type Agpl.Types.Unsigned_8;
      Acum : Natural := 0;
   begin
      Acum := Natural (F.Red_Value (R, C)) +
        Natural (F.Green_Value (R, C)) +
        Natural (F.Blue_Value (R, C));

      return Agpl.Types.Unsigned_8 (Acum / 3);
   end Avg_Value;

   ---------
   -- "N" --
   ---------

   function N (This : Png_File; X : Integer) return Agpl.Types.Unsigned_8 is
   begin
--        if X > 0 then
--           Log ("pre-norm:" & X'Img, Always);
--        end if;
      return Agpl.Types.Unsigned_8 (Float'Floor (Float (X) / This.Norm));
   end N;

   -----------
   -- Width --
   -----------

   function  Width (F : Png_File) return Dimension is
   begin
      return Pio.Width (F.Png);
   end Width;

   ------------
   -- Height --
   ------------

   function Height (F : Png_File) return Dimension is
   begin
      return Pio.Height (F.Png);
   end Height;

end Agpl.Png;
