private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Agpl.Drawing.Buffer is

   --  This doesn't draw anything, but instead allows to keep a copy of what
   --   a drawer is supposed to do.

   pragma Preelaborate;

   --  This type is the key abstraction to be used around...
   --  It can be flushed into another drawer, or it can be passed as a drawable
   --    to another drawer (useful when a drawable is expected)
   type object is new Drawer and Drawable with private;

   overriding
   procedure Draw_Line (This   : in out Object;
                        X1, Y1,
                        X2, Y2 : Float);

   overriding
   procedure Fill_Rectangle
     (This   : in out Object;
      X1, Y1,
      X2, Y2 : Float);

   overriding
   procedure Set_Color (This  : in out Object;
                        Rgb   :        Types.Rgb_Triplet;
                        Alpha :        Types.Unsigned_8);

   overriding
   procedure Write (This : in out Object;
                    X, Y : Float;
                    Utf8 : String);

   not overriding
   procedure Flush (This  :        Object;
                    Dest  : in out Drawer'Class);

   not overriding
   procedure Clear (This : in out Object);
   --  Start afresh

   not overriding
   procedure Flush_And_Clear (This  : in out Object;
                              Dest  : in out Drawer'Class);
   --  Send to another drawer (@dest@) the buffered commands,
   --    and optionally clear them to start afresh.

   overriding
   procedure Draw (This :        Object;
                   Dest : in out Drawer'Class);
   --  Flush to Dest

private

   type Action is abstract tagged null record;

   procedure Perform (This :        Action;
                      Dest : in out Drawer'Class) is abstract;
   --  Perform for real!

   package Action_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Action'Class);

   type Object is new Drawer and Drawable with record
      Actions : Action_Lists.List;
   end record;

   type Action_Line is new Action with record
      X1, Y1, X2, Y2 : Float;
   end record;

   overriding
   procedure Perform (This :        Action_Line;
                      Dest : in out Drawer'Class);

   type Action_Fill is new Action with record
      X1, Y1, X2, Y2 : Float;
   end record;

   overriding
   procedure Perform (This :        Action_Fill;
                      Dest : in out Drawer'Class);

   type Action_Color is new Action with record
      Rgb   : Types.Rgb_triplet;
      Alpha : Types.Unsigned_8;
   end record;

   overriding
   procedure Perform (This :        Action_Color;
                      Dest : in out Drawer'Class);

   type Action_Write (Length : Natural) is new Action with record
      X, Y : Float;
      Utf8 : String (1 .. Length);
   end record;

   overriding
   procedure Perform (This :        Action_Write;
                      Dest : in out Drawer'Class);

end Agpl.Drawing.Buffer;
