with Agpl.Drawing.Buffer;
with Agpl.Transf2D;

package Agpl.Drawing.Transformations is

   --  Classes for transforming input actions.

   pragma Preelaborate;

   type Transformer is new Drawer with private;
   --  Uses the two above to provide some high-level capacities

   procedure Clear (This : in out Transformer);
   --  Forget transformation and previous drawing actions.

   procedure Fit (This     : in out Transformer;
                  Into     : in out Drawer'Class;
                  X_Left,
                  X_Right,
                  Y_Bottom,
                  Y_Top    :        Float;
                  Square   :        Boolean := True);
   --  Flush things drawn here fitting them to the given area.
   --  Optionally drop aspect ratio

private

   package Transf is new Transf2D (Float); use Transf;

   type Modes is (Filtering, Drawing);

   type Transformer is new Drawer with record
      Mode       : Modes := Filtering;

      Store      : Buffer.Object;
      Back       : Drawer_Access;

      Xmin, Ymin : Float := Float'Last;
      Xmax, Ymax : Float := Float'First;
      --  Used during filtering

      T          : Transf.Transformation;
   end record;

   overriding
   procedure Draw_Line (This   : in out Transformer;
                        X1, Y1,
                        X2, Y2 : Float);

   overriding
   procedure Fill_Rectangle
     (This   : in out Transformer;
      X1, Y1,
      X2, Y2 : Float);

   overriding
   procedure Set_Color (This  : in out Transformer;
                        Rgb   :        Types.Rgb_Triplet;
                        Alpha :        Types.Unsigned_8);

   overriding
   procedure Write (This : in out Transformer;
                    X, Y : Float;
                    Utf8 : String);

   not overriding
   procedure Get_Ranges (This : in out Transformer; X, Y : Float);

end Agpl.Drawing.Transformations;
