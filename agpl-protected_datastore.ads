--  A collection of protected data with notifiers

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

package Agpl.Protected_Datastore is

   pragma Preelaborate;

   Data_Not_Present : exception;

   type Object_Data is abstract tagged null record;
   --  This is data to be kept in the datastore
   type Object_Data_Access is access all Object_Data'Class;

   subtype Object_Key is String;
   --  This is the indexing type

   type Key_Listener is interface; -- Something that wants to be notified
                                   --  when a key is stored
   type Key_Listener_Access is access all Key_Listener'Class;
   procedure On_Key_Stored (This  : in out Key_Listener;
                            Key   : in     Object_Key;
                            Value : in     Object_Data'Class)
   is abstract;
   --  Processing here should be as fast as possible, since all chained calls
   --  are synchronous.
   --  Care is to be taken to not cause recursive calling!! This is up to the
   --  clients.

   type Object is tagged limited private;
   type Object_Access is access Object'Class;

   function Contains (This : in Object;
                      Key  : in Object_Key)
                      return    Boolean;

   function Get (This : in Object;
                 Key  : in Object_Key)
                 return    Object_Data'Class;
   --  Retrieve something or raise Data_Not_Present.

   procedure Put (This  : in out Object;
                  Key   : in     Object_Key;
                  Value : in     Object_Data'Class);
   --  Store something.

   procedure Set (This  : in out Object;
                  Key   : in     Object_Key;
                  Value : in     Object_Data'Class) renames Put;

   procedure Listen (This     : in out Object;
                     Key      : in     Object_Key;
                     Listener : not null Key_Listener_Access);
   --  Register for a key

private

   package Listener_Vectors is new Ada.Containers.Vectors
     (Positive,
      Key_Listener_Access);

   package Key_Listener_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Object_Key,
      Listener_Vectors.Vector,
      "<",
      Listener_Vectors."=");

   package Key_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Object_Key,
      Object_Data'Class);

   protected type Safe_Object is
      function Contains (Key : in Object_Key)
                         return   Boolean;

      procedure Put (Key   : in Object_Key;
                     Value : in Object_Data'Class);

      function Get (Key : in Object_Key)
                    return   Object_Data'Class;

      function Get_Callbacks (Key : in Object_Key)
                              return   Listener_Vectors.Vector;

      procedure Listen (Key      : in Object_Key;
                        Listener : Key_Listener_Access);

   private
      Callbacks : Key_Listener_Maps.Map;
      Values    : Key_Object_Maps.Map;
   end Safe_Object;

   type Object is tagged limited record
      Safe : Safe_Object;
   end record;

end Agpl.protected_Datastore;
