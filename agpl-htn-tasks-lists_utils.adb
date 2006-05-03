package body Agpl.Htn.Tasks.Lists_Utils is

   use Lists;

   -----------------
   -- Concatenate --
   -----------------

   procedure Concatenate (Dest : in out Lists.List; Src : in Lists.List) is

      procedure Append (X : in Cursor) is
      begin
         Dest.Append (Element (X));
      end Append;

   begin
      Iterate (Src, Append'Access);
   end Concatenate;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Src : in Lists.List) return Vectors.Vector is
      Dst : Vectors.Vector;
      I   : Lists.Cursor := Lists.First (Src);
   begin
      while Lists.Has_Element (I) loop
         Vectors.Append (Dst, Lists.Element (I));
         Lists.Next (I);
      end loop;

      return Dst;
   end To_Vector;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Src : in Maps.Map) return Vectors.Vector is
   Dst : Vectors.Vector;
      I   : Maps.Cursor := Maps.First (Src);
   begin
      while Maps.Has_Element (I) loop
         Vectors.Append (Dst, Maps.Element (I));
         Maps.Next (I);
      end loop;

      return Dst;
   end To_Vector;

end Agpl.Htn.Tasks.Lists_Utils;
