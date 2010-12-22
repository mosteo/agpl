package body Agpl.Containers.Protected_Indefinite_Doubly_Linked_Lists is

   --------------------
   -- Protected_List --
   --------------------

   protected body Protected_List is

   -----------
   -- Clear --
   -----------

      procedure Clear is
      begin
         Internal.Clear;
      end Clear;

      ------------
      -- Append --
      ------------

      procedure Append
        (New_Item  : Element_Type;
         Count     : Count_Type := 1) is
      begin
         Internal.Append (New_Item, Count);
      end Append;

      ------------------
      -- Delete_First --
      ------------------

      procedure Delete_First (Count : Count_Type := 1) is
      begin
         Internal.Delete_First (Count);
      end Delete_First;

      -------------------
      -- First_Element --
      -------------------

      function First_Element return Element_Type is
      begin
         return Internal.First_Element;
      end First_Element;

      ----------------------------
      -- First_Element_Blocking --
      ----------------------------

      entry First_Element_Blocking (Item : out Element_Type)
        when not Internal.Is_Empty is
      begin
         Item := Internal.First_Element;
      end First_Element_Blocking;

      ---------------------------------------
      -- First_Element_Blocking_And_Delete --
      ---------------------------------------

      entry First_Element_Blocking_And_Delete (Item : out Element_Type)
        when not Internal.Is_Empty is
      begin
         Item := Internal.First_Element;
         Internal.Delete_First;
      end First_Element_Blocking_And_Delete;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Internal.Is_Empty;
      end Is_Empty;

      ------------
      -- Length --
      ------------

      function Length return Count_Type is
      begin
         return Internal.Length;
      end Length;

   end Protected_List;

end Agpl.Containers.Protected_Indefinite_Doubly_Linked_Lists;
