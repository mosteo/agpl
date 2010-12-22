with Gnat.Heap_Sort;

package body Agpl.Average_queue is

   ----------
   -- Push --
   ----------

   procedure Push (this: in out Object; New_item: in Item) is
   begin
--        This.Sum := This.Sum + New_Item;
--
--        if This.Length >= This.Data'Length then
--           Remove item to be dropped from acummulator:
--           This.Sum := This.Sum - This.Data (This.Pos);
--        end if;

      This.Data (This.Pos) := New_Item;
      This.Pos             := This.Pos + 1;

      if This.Pos > This.Data'Last then
         This.Pos := This.Data'First;
      end if;

      if This.Length < This.Data'Length then
         This.Length := This.Length + 1;
      end if;
   end Push;

   ---------
   -- Sum --
   ---------

   function Sum (This : in Object) return Item'Base is
      Acum : Item'Base := 0.0;
   begin
      for I in This.Data'First .. This.Data'First + This.Length - 1 loop
         Acum := Acum + This.Data (I);
      end loop;
      return Acum;
   end Sum;

   -------------
   -- Average --
   -------------

   function Average (This : in Object) return Item is
   begin
      if this.Length = 0 then
         raise No_data;
      else
--           Put_Line ("Sum: " & This.Sum'Img);
--           Put_Line ("Len: " & This.Length'Img);
--           Put_Line ("Avg: " & Item'Image (This.Sum / Item'Base (This.Length)));
         return
           This.Sum /
           Item'Base (This.Length);
      end if;
   end Average;

   ------------
   -- Median --
   ------------

   function Median (This : in Object) return Item is
   begin
      return This.Percentil (50);
   end Median;

   ---------------
   -- Percentil --
   ---------------

   function Percentil (This : in Object; Pct : Pctil_Range) return Item is
      pragma Assert (This.Data'First = 1);

      Sorted : Data_Array := This.Data
        (This.Data'First .. This.Data'First + This.Length - 1);
      pragma Assert (Sorted'First = 1);

      procedure Swap (I, J : Natural) is
         T : constant Item := Sorted (I);
      begin
         Sorted (I) := Sorted (J);
         Sorted (J) := T;
      end Swap;

      function Less (I, J : Natural) return Boolean is
      begin
         return Sorted (I) < Sorted (J);
      end Less;
   begin
      if This.Length = 0 then
         raise Constraint_Error with "No data, no percentil!";
      end if;

      Gnat.Heap_Sort.Sort (Sorted'Last,
                           Swap'Unrestricted_Access,
                           Less'Unrestricted_Access);
      return Sorted (Sorted'First + ((Sorted'Length - 1) * Pct / 100));
   end Percentil;

   ----------
   -- Last --
   ----------

   function Last (This : in Object) return Item is
      Idx : Positive;
   begin
      if This.Length = 0 then
         raise Constraint_Error with "No data, no last!";
      else
         if This.Pos = This.Data'First then
            Idx := This.Data'Last;
         else
            Idx := This.Pos - 1;
         end if;
         return This.Data (Idx);
      end if;
   end Last;

   --------------
   -- Is_empty --
   --------------

   function Is_empty (This : in Object) return Boolean is
   begin
      return This.Length = 0;
   end Is_empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : out Object) is
   begin
      This.Length := 0;
      This.Pos    := 1;
--        This.Sum    := 0.0;
   end Clear;

end Agpl.Average_queue;
