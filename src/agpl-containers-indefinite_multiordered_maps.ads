--  A table with two indexing/ordering key types

with Ada.Containers.Ordered_Maps,
     Agpl.Generic_Handle;

generic
   type Key_Type is private;
   type Indices is (<>);
   type Element_Type (<>) is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Agpl.Containers.Indefinite_Multiordered_Maps is

   pragma Preelaborate;

   type Key_Array is array (Indices) of Key_Type;

   type Map is tagged private;

   type Cursor (Index : Indices) is tagged private;

   function Has_Element (I : Cursor) return Boolean;
   pragma Inline (Has_Element);

   function Element (I : Cursor) return access Element_Type;
   pragma Inline (Element);

   function Element (Container : Map;
                     Index     : Indices;
                     Key       : Key_Type) return access Element_Type;

   function Key (I : Cursor) return Key_Type;
   pragma Inline (Key);

   function Is_Empty (Container : Map) return Boolean;
   pragma Inline (Is_Empty);

   function Length (Container : Map) return Natural;
   pragma Inline (Length);

   function Find (Container : Map;
                  Index     : Indices;
                  Key       : Key_Type) return Cursor'Class;

   function First (Container : Map;
                   Index     : Indices) return Cursor'Class;
   pragma Inline (First);

   function First_Element (Container : Map;
                           Index     : Indices) return access Element_Type;

   function Last (Container : Map;
                  Index     : Indices) return Cursor'Class;
   pragma Inline (Last);

   procedure Next (This : in out Cursor);
   pragma Inline (Next);

   function Previous (This : Cursor) return Cursor;
   pragma Inline (Previous);

   procedure Previous (This : in out Cursor);
   pragma Inline (Previous);

   procedure Include (Container : in out Map;
                      Key       :        Key_Array;
                      Element   :        Element_Type);

   procedure Exclude (Container : in out Map;
                      Index     :        Indices;
                      Key       :        Key_Type);

   procedure Delete (Container : in out Map;
                     I         : in out Cursor'Class);

   procedure Iterate_Query
     (Container :        Map;
      Process   : access procedure (Key     : Key_Type;
                                    Element : Element_Type);
      Index     :        Indices := Indices'First);

   procedure Iterate_Update
     (Container : in out Map;
      Process   : access procedure (Key     : Key_Type;
                                    Element : in out Element_Type);
      Index     :        Indices := Indices'First);

private

   type Inner_Element;

   type Inner_Access is access all Inner_Element;

   function "=" (L, R : Inner_Access) return Boolean;
   --  Maps to element."="

   type Element_Access is access all Element_Type;

   package Handles is new Generic_Handle (Element_Type);

   package Maps is
     new Ada.Containers.Ordered_Maps (Key_Type,
                                      Inner_Access);

   type Map_Array is array (Indices) of Maps.Map;

   type Cursor_Array is array (Indices) of Maps.Cursor;

   type Inner_Element is record
      Element : Handles.Object;
      Cursors : Cursor_Array;
   end record;

   --   1: Create ptr to new Inner_Element
   --   2: Insert ptr into each container, updating with the cursor obtained.

   --   Now we have direct access by cursor.
   --   We can delete, self-removing from all containers
   --   We can get the first element by all indices.

   type Map is tagged record
      M : Map_Array;
   end record;

   type Cursor (Index : Indices) is tagged record
      Cursor : Maps.Cursor;
   end record;

end Agpl.Containers.Indefinite_Multiordered_Maps;
