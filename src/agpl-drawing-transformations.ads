package Agpl.Drawing.Transformations is

   --  Classes for transforming input actions.

   pragma Preelaborate;

   type Scaler is new Drawer with private;

   type Mover is new Drawer with private;

   type Transformer is new Drawer with private;
   --  Uses the two above to provide some high-level capacities

private

   type Scaler is new Drawer with null record;

   type Mover is new Drawer with null record;

   type Transformer is new Drawer with null record;

   procedure Draw_Line (This   : in out Scaler;
                        X1, Y1,
                        X2, Y2 : Float);

   procedure Fill_Rectangle
     (This   : in out Scaler;
      X1, Y1,
      X2, Y2 : Float);

   procedure Set_Color (This  : in out Scaler;
                        Rgb   :        Types.Rgb_Triplet;
                        Alpha :        Types.Unsigned_8);

   procedure Write (This : in out Scaler;
                    X, Y : Float;
                    Utf8 : String);

end Agpl.Drawing.Transformations;
