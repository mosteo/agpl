

with Agpl.Http.Server.Sort_handler;
with Agpl.Http.Server.Sort_handler.Aux;

with Ada.Unchecked_deallocation;
use  Ada;
with System.Address_to_access_conversions;

package body Agpl.Http.Server.Sort_handler.Simple_list is

   type Object_access is access all Object;

   package Conv is new System.Address_to_access_conversions (Object);

   use Aux.Address_lists;

   Msgs    : Aux.Address_lists.List;
   Pending : Natural := 0;
   pragma Atomic (Pending);

   protected Safe is
   procedure Add (This : in Object);
   procedure Clear;
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set);
   end Safe;

   protected body Safe is
   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object) is
      Auxp : Object_access;
      procedure Free is new Unchecked_deallocation (Object, Object_access);
   begin
      while Integer (Msgs.Length) >= Max_entries loop
         Auxp := Object_access (Conv.To_pointer (Element (Last (Msgs))));
         Free (Auxp);
         Delete_last (Msgs);
      end loop;
      Auxp := new Object'(This);
      Prepend (Msgs, Conv.To_address (Conv.Object_pointer (Auxp)));
      Pending := Pending + 1;
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Clear (Msgs);
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      use Agpl.Http.Server.Sort_handler;
      I    : Cursor := First (Msgs);
      Pos  : Positive      := 1;
   begin
      while Has_Element (I) loop
         declare
            Row : Data_row;
            Q   : System.Address renames Element (I);
         begin
            Generate_row (
               Object_access (Conv.To_pointer (Q)).all,
               Pos <= Pending,
               Row);
            Append (Data, Row);
            I := Next (I);
         end;
         Pos := Pos + 1;
      end loop;
      Pending := 0;
   end Http_report;
   end Safe;

   ------------------------------------------------------------------------
   -- New_events                                                         --
   ------------------------------------------------------------------------
   -- Says how many new events are there since last check.
   function New_events return Natural is
   begin
      return Pending;
   end New_events;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object) is
   begin
      Safe.Add (This);
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Safe.Clear;
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
   begin
      Safe.Http_report (Data);
   end Http_report;


end Agpl.Http.Server.Sort_handler.Simple_list;
