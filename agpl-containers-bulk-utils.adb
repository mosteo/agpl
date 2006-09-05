package body Agpl.Containers.Bulk.Utils is

   -----------------
   -- Concatenate --
   -----------------

   procedure Concatenate (Dest : in out Lists.List; Src : in Lists.List) is

      use Lists;

      procedure Append (X : in Cursor) is
      begin
         Dest.Append (Element (X));
      end Append;

   begin
      Iterate (Src, Append'Access);
   end Concatenate;

   -------------
   -- To_List --
   -------------

   function To_List (Src : in Vectors.Vector) return Lists.List is
      Dst : Lists.List;

      procedure Add (I : Vectors.Cursor) is
      begin
         Dst.Append (Vectors.Element (I));
      end Add;
   begin
      Src.Iterate (Add'Access);
      return Dst;
   end To_List;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Src : in Lists.List) return Vectors.Vector is
      Dst : Vectors.Vector;

      procedure Add (I : Lists.Cursor) is
      begin
         Dst.Append (Lists.Element (I));
      end Add;
   begin
      Src.Iterate (Add'Access);
      return Dst;
   end To_Vector;

end Agpl.Containers.Bulk.Utils;
