package Agpl.Gui is

   pragma Pure;

   type Event is interface;

   type Event_Handler is interface;

   procedure Triggered (Listener : in out Event_Handler; E : Event'Class)
   is abstract;
   --  Called on event occurrence

   --  Some events used elsewhere  --

   type Clicked is new Event with record
      Data_X, Data_Y : Float;
      --  This corresponds to Data coordinates, no screen coordinates
   end record;

end Agpl.Gui;
