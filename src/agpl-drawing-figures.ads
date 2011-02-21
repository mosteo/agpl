with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics.Real_Arrays;

with Agpl.Drawing.Buffer;

package Agpl.Drawing.Figures is

   pragma Preelaborate;

   package RA renames Ada.Numerics.Real_Arrays;

   subtype Confidence is Float range 0.0 .. 1.0;

   function Null_Figure return Drawable'Class;

   type Vprogress is new Drawable with record
      X, Y,
      Width,
      Height,
      Max,
      Val        : Float;
      Text_Above : Boolean := True;
      Show_Pct   : Boolean := True;
   end record;
   procedure Draw (This : Vprogress; Into : in out Drawer'Class);

   function Segment (X1, Y1, X2, Y2 : Float) return Buffer.Object;

   function Target (X, Y : Float; Size : Float := 1.0) return Buffer.Object;
   --  A big X is marking the treasure

   function Text (X, Y : Float; S : String) return Drawable'Class;

   type Bivariate_Ellipse (<>) is new Drawable with private;
   procedure Draw (This : Bivariate_Ellipse; Into : in out Drawer'Class);

   subtype Point is Ra.Real_Vector (1 .. 2);
   subtype Cov2d is Ra.Real_Matrix (1 .. 2, 1 .. 2);

   function Create_Bivariate_Ellipse
     (Mu     : Point;              -- Mean of distribution.
      Th     : Float;              -- Angle of whatever this Mu corresponds.
      Sigma  : Cov2d;              --  Covariance of distro.
                                   --  This is a symmetric positive semi-definite matrix.
      Conf   : Confidence := 0.95; -- Percent of probability mass inside the ellipse
      Curve  : Boolean := True;    -- Draw ellipse
      Axes   : Boolean := True;    -- Draw axes
      Points : Positive := 20      -- Points to approximate ellipse
     ) return Bivariate_Ellipse;
   --  Adapted from
   --  http://openslam.informatik.uni-freiburg.de/data/svn/tjtf/trunk/matlab/plotcov2.m

   function Robot (X, Y, A : Float;
                   Size    : Float := 0.5) return Drawable'Class;

   type Float_Array is array (Positive range <>) of Float;

   function Simple_History (Values   : Float_Array)
                            return     Drawing.Buffer.Object;
   --  Each sample is 1.0 units width

   package Float_Lists is new Ada.Containers.Doubly_Linked_Lists (Float);
   type Float_List is new Float_Lists.List with null record;

   procedure Add_Sample (List : in out Float_List; Sample : Float);
   procedure Trim (List : in out Float_List; Samples : Natural);
   --  Remove excess List.First_Element (i.e. new samples go to tail)

   function Simple_History (Values   : Float_List)
                            return     Drawing.Buffer.Object;
   --  Each sample is 1.0 units width

   function Simple_History (Min, Max : Float;
                            Values   : Float_List;
                            Max_Vals : Natural;
                            Val_RGB  : Types.Rgb_Triplet := (0, 0, 255);
                            Axis_RGB : Types.Rgb_triplet := (0, 0, 0))
                            return     Drawing.Buffer.object;
   --  As above, but all-in-one.
   --  Max vals is used to make the rangespan in the axis

private

   type Bivariate_Ellipse is new Drawable with record
      Mu     : Point;
      Th     : Float;
      Sigma  : Cov2d;
      Conf   : Confidence;
      Curve  : Boolean;
      Axes   : Boolean;
      Points : Positive;
   end record;

end Agpl.Drawing.Figures;
