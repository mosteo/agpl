package body Agpl.Cr.Map.Grid is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : in Coordinates) return Boolean is
   begin
      return L.X < R.X or else (L.X = R.X and then L.Y < R.Y);
   end "<";

   ------------
   -- Create --
   ------------

   function Create (Resolution : in Float) return Object is
      This : Object;
   begin
      This.Res := Resolution;

      return This;
   end Create;

   ------------
   -- Get_At --
   ------------

   function Get_At
     (This : in Object;
      X, Y : in Float)
      return Observations'Class
   is
      I : constant Cursor := Find (This.Cells, Key (This, X, Y));
   begin
      if I /= No_Element then
         return Element (I);
      else
         raise No_Data;
      end if;
   end Get_At;

   --------------
   -- Is_Known --
   --------------

   function Is_Known (This : in Object;
                      X, Y : in Float) return Boolean
   is
   begin
      return Contains (This.Cells, Key (This, X, Y));
   end Is_Known;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (This : in out Object;
      X, Y : in     Float;
      Obs  : in     Observations'Class)
   is
   begin
      Include (This.Cells, Key (This, X, Y), Obs);
   end Set_At;

   ---------
   -- Key --
   ---------

   function Key (This : in Object; X, Y : in Float) return Coordinates is
   begin
      return
        (X => Integer (Float'Floor (X / This.Res)),
         Y => Integer (Float'Floor (Y / This.Res)));
   end Key;

end Agpl.Cr.Map.Grid;
