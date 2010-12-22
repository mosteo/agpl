with Ada.Containers.Indefinite_Ordered_Sets;

package body Agpl.Containers.Indefinite_Matrix2d is

   use Index_Index_Value_Maps;

   package IVM renames Index_Value_Maps;

   --------------
   -- Contains --
   --------------

   function Contains (This : Matrix; R, C : Index) return Boolean is
      X      : constant access Matrix := This'Unrestricted_Access;
      --  Necessary for Update_Element
      --  Safe since Matrix is tagged and this is used locally

      I      : constant Cursor  := This.Find (R);
      Result :          Boolean := False;
   begin
      if not Has_Element (I) then
         return False;
      end if;

      declare
         procedure Contains (Key : Index; RMap : in out IVM.Map) is
            pragma Unreferenced (Key);
         begin
            Result := RMap.Contains (C);
         end Contains;
      begin
         X.Update_Element (I, Contains'Access);
      end;

      return Result;
   end Contains;

   ---------
   -- Get --
   ---------

   function Get (This : Matrix; R, C : Index) return Value is
      X : constant access Matrix := This'Unrestricted_Access;
      I : constant Cursor := This.Find (R);
      J :          IVM.Cursor;

      procedure Get (Key : Index; RMap : in out IVM.Map) is
         pragma Unreferenced (Key);
      begin
         J := RMap.Find (C);
      end Get;
   begin
      X.Update_Element (I, Get'Access);
      return IVM.Element (J);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (This : Matrix;
      R, C : Index;
      V : out Value;
      Found : out Boolean)
   is
      X : constant access Matrix := This'Unrestricted_Access;
      I : constant Cursor := This.Find (R);
      J : IVM.Cursor;

      procedure Get (Key : Index; RMap : in out IVM.Map) is
         pragma Unreferenced (Key);
      begin
         J := RMap.Find (C);
         if IVM.Has_Element (J) then
            V     := IVM.Element (J);
            Found := True;
         else
            Found := False;
         end if;
      end Get;
   begin
      if Has_Element (I) then
         X.Update_Element (I, Get'Access);
      else
         Found := False;
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Matrix; R, C : Index; V : Value) is
      I  : Cursor  := This.Find (R);
      Ok : Boolean := False;

      procedure Set (Key : Index; RMap : in out IVM.Map) is
         pragma Unreferenced (Key);
      begin
         RMap.Include (C, V);
      end Set;
   begin
      if not Has_Element (I) then
         This.Insert (R, IVM.Empty_Map, I, Ok);
         if not Ok then
            raise Constraint_Error with "Row creation failed";
         end if;
      end if;

      This.Update_Element (I, Set'Access);
   end Set;

   -----------------
   -- Row_Indexes --
   -----------------

   function Row_Indexes (This : Matrix) return Index_Vector is
      Result : Index_Vector;

      procedure X (I : Cursor) is
      begin
         Result.Append (Key (I));
      end X;
   begin
      This.Iterate (X'Access);
      return Result;
   end Row_Indexes;

   --------------------
   -- Column_Indexes --
   --------------------

   function Column_Indexes (This : Matrix) return Index_Vector is
      Thix : constant access Matrix := This'Unrestricted_Access;

      package Index_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Index);

      Set : Index_Sets.Set;

      procedure X (I : Cursor) is
         procedure Y (Key : Index; Val : in out Index_Value_Maps.Map) is
            pragma Unreferenced (Key);
            procedure Z (I : IVM.Cursor) is
            begin
               Set.Include (IVM.Key (I));
            end Z;
         begin
            Val.Iterate (Z'Access);
         end Y;
      begin
         Thix.Update_Element (I, Y'Access);
      end X;
   begin
      This.Iterate (X'Access);

      declare
         Result : Index_Vector;
         procedure X (I : Index_Sets.Cursor) is
         begin
            Result.Append (Index_Sets.Element (I));
         end X;
      begin
         Set.Iterate (X'Access);
         return Result;
      end;
   end Column_Indexes;

end Agpl.Containers.Indefinite_Matrix2d;
