private with Ada.Containers.Indefinite_Ordered_Maps;
private with Agpl.Drawing.Buffer;

package Agpl.Drawing.Multisource is

   pragma Preelaborate;

   type Object is limited new Drawable with private;
   --  This type is thread safe

   type Object_Access is access all Object'Class;

   procedure Draw (This : in out Object;
                   Id   :        String;
                   What :        Drawable'Class);
   --  Store or replace under Id the drawable primitives.
   --  What may dissapear afterwards.
   --  NOTE: things are drawn in lexicographical order of ID

   procedure Flush (This :        Object;
                    Dest : in out Drawer'Class);

   procedure Clear (This : in out Object);

   procedure Clear (This : in out Object;
                    Id   :        String);

   overriding
   procedure Draw (This :        Object;
                   D    : in out Drawer'Class) renames Flush;

   function Is_Dirty (This : Object) return Boolean;
   --  If any clear/draw after the last flush

private

   package Id_Buffer_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Buffer.Object'Class,
                                                 "<", Buffer."=");

   protected type Safe_Type is
      procedure Draw (Id   :        String;
                      What :        Drawable'Class);
      --  Store or replace under Id the drawable primitives.
      --  What may dissapear afterwards.

      procedure Flush (Dest : in out Drawer'Class);

      procedure Clear;

      procedure Clear (Id   :        String);

      function Is_Dirty return Boolean;

   private
      Dirty   : Boolean := False;
      Buffers : Id_Buffer_Maps.Map;
   end Safe_Type;

   type Object is limited new Drawable with record
      Self : access Object := Object'Unchecked_Access;

      Safe : Safe_Type;
   end record;

end Agpl.Drawing.Multisource;
