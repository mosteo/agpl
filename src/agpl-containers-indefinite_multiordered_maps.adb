with Ada.Unchecked_Deallocation;

package body Agpl.Containers.Indefinite_Multiordered_Maps is

   procedure Free is
     new Ada.Unchecked_Deallocation (Inner_Element, Inner_Access);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.M (Indices'First).Is_Empty;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Natural is
   begin
      return Natural (Container.M (Indices'First).Length);
   end Length;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      Index     : Indices;
      Key       : Key_Type)
      return access Element_Type
   is
   begin
      return Container.M (Index).Element (Key).Element.Ref;
   end Element;

   ----------
   -- Find --
   ----------

   function Find (Container : Map;
                  Index     : Indices;
                  Key       : Key_Type) return Cursor'Class is
   begin
      return Cursor'(Index  => Index,
                     Cursor => Container.M (Index).Find (Key));
   end Find;

   -------------------
   -- First_Element --
   -------------------

   function First_Element
     (Container : Map;
      Index     : Indices)
      return access Element_Type
   is
   begin
      return Container.M (Index).First_Element.Element.Ref;
   end First_Element;

   -------------
   -- Include --
   -------------

   procedure Include
     (Container : in out Map;
      Key       :        Key_Array;
      Element   :        Element_Type)
   is
      Inner : constant Inner_Access :=
                new Inner_Element'(Element => Handles.Set (Element),
                                   others  => <>);
   begin
      for I in Indices loop
         Container.M (I).Include (Key (I), Inner);
         Inner.Cursors (I) := Container.M (I).Find (Key (I));
      end loop;
   end Include;

   -------------
   -- Exclude --
   -------------

   procedure Exclude
     (Container : in out Map;
      Index     :        Indices;
      Key       :        Key_Type)
   is
      Inner : Inner_Access := Container.M (Index).Element (Key);
   begin
      for I in Inner.Cursors'Range loop
         Container.M (I).Delete (Inner.Cursors (I));
      end loop;
      Free (Inner);
   end Exclude;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Inner_Access) return Boolean is
      use Handles;
   begin
      return L.Element = R.Element;
   end "=";

   -----------
   -- First --
   -----------

   function First (Container : Map;
                   Index     : Indices) return Cursor'Class is
   begin
      return Cursor'(Index  => Index,
                     Cursor => Container.M (Index).First);
   end First;

   ----------
   -- Last --
   ----------

   function Last (Container : Map;
                  Index     : Indices) return Cursor'Class is
   begin
      return Cursor'(Index  => Index,
                     Cursor => Container.M (Index).Last);
   end Last;

   ----------
   -- Next --
   ----------

   procedure Next (This : in out Cursor) is
   begin
      Maps.Next (This.Cursor);
   end Next;

   --------------
   -- Previous --
   --------------

   procedure Previous (This : in out Cursor) is
   begin
      Maps.Previous (This.Cursor);
   end Previous;

   --------------
   -- Previous --
   --------------

   function Previous (This : Cursor) return Cursor is
   begin
      return Cursor'(Index  => This.Index,
                     Cursor => Maps.Previous (This.Cursor));
   end Previous;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (I : Cursor) return Boolean is
   begin
      return Maps.Has_Element (I.Cursor);
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (I : Cursor) return access Element_Type is
   begin
      return Maps.Element (I.Cursor).Element.Ref;
   end Element;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in out Map;
                     I         : in out Cursor'Class)
   is
   begin
      Container.Exclude (I.Index, Maps.Key (I.Cursor));
   end Delete;

   function Key (I : Cursor) return Key_Type is
   begin
      return Maps.Key (I.Cursor);
   end Key;

   --------------------
   -- Iterate_Update --
   --------------------

   procedure Iterate_Update
     (Container : in out Map;
      Process   : access procedure (Key     : Key_Type;
                                    Element : in out Element_Type);
      Index     :        Indices := Indices'First)
   is
      procedure Iterate (I : Maps.Cursor) is
      begin
         Process (Maps.Key (I), Maps.Element (I).Element.Ref.all);
      end Iterate;
   begin
      Container.M (Index).Iterate (Iterate'Access);
   end Iterate_Update;

   -------------------
   -- Iterate_Query --
   -------------------

   procedure Iterate_Query
     (Container :        Map;
      Process   : access procedure (Key     : Key_Type;
                                    Element : Element_Type);
      Index     :        Indices := Indices'First)
   is
      procedure Iterate (I : Maps.Cursor) is
      begin
         Process (Maps.Key (I), Maps.Element (I).Element.Ref.all);
      end Iterate;
   begin
      Container.M (Index).Iterate (Iterate'Access);
   end Iterate_Query;

end Agpl.Containers.Indefinite_Multiordered_Maps;
