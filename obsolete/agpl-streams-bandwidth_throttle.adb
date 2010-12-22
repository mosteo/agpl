 

--  Filter stream which uses an Agpl.Bandwidth_Throttle to limit data rates.
--  Provides statistics about global data rate and "instantaneous" data rate.
--  The instantaneous data rate averaging can be configured.

with Ada.Streams;

with Text_Io;

package body Agpl.Streams.Bandwidth_Throttle is

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   --  Reserves from the throttle and reads that from back.
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
      Aw, Ex : Stream_Element_Count;
   begin
      This.Throttle_In.Commit (Item'Length, Aw);
      if Aw < Item'Length then
         This.Throttle_In.Commit (Item'Length - Aw, Ex, Extra => true);
      else
         Ex := 0;
      end if;

      Ada.Streams.Read (
         This.Back.all,
         Item (Item'First .. Item'First + Aw + Ex - 1),
         Last);

      This.Total_Read := This.Total_Read + Last - Item'First + 1;
      Avg.Push (This.Avg_In, Float (Last - Item'First + 1));
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   -- Non-blocking. Will cache data which cannot be immediately written.
	procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array)
   is
      Aw, Ex : Stream_Element_Count;
   begin
      This.Throttle_Out.Commit (Item'Length, Aw);
      if Aw < Item'Length then
         This.Throttle_Out.Commit (Item'Length - Aw, Ex, Extra => true);
      else
         Ex := 0;
      end if;

      Text_Io.Put_Line ("Aw:" & Aw'Img & " Ex:" & Ex'Img & " It:" &
         Natural'Image (Item'Length));

      Ada.Streams.Write (
         This.Back.all,
         Item (Item'First .. Item'First + Aw + Ex - 1));
      This.Total_Written := This.Total_Written + Aw + Ex;
      Avg.Push (This.Avg_Out, Float (Aw + Ex));

      if Aw + Ex < Item'Length then
         Circular.Write (
            This.Buf_Out,
            Item (Item'First + Aw + Ex .. Item'Last));
      end if;

      if Circular.Available_Read (This.Buf_Out) > Stream_Element_Count'(0) then
         Flush (This);
      end if;
   end Write;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   -- Cleans everything inside, including stats.
   procedure Reset (This : in out Stream_Type) is
   begin
      Circular.Reset (This.Buf_Out);
      This.Total_Read    := 0;
      This.Total_Written := 0;
      This.Start         := Calendar.Clock;
   end Reset;

   ------------------------------------------------------------------------
   -- Flush                                                              --
   ------------------------------------------------------------------------
   -- Tries to write again data which was cached.
   procedure Flush (This : in out Stream_type) is
      Avail  : constant Stream_Element_Count := Circular.Available_Read (This.Buf_Out);
      Aw, Ex : Stream_Element_Count;
   begin
      if Avail > 0 then
         This.Throttle_Out.Commit (Avail, Aw);
         if Aw < Avail then
            This.Throttle_Out.Commit (Avail - Aw, Ex, Extra => true);
         else
            Ex := 0;
         end if;
         Text_Io.Put_Line ("Aw:" & Aw'Img & " Ex:" & Ex'Img);
         declare
            Item : Stream_Element_Array (1 .. Aw + Ex);
            Last : Stream_Element_Offset;
         begin
            Circular.Peek (This.Buf_Out, Item, Last);
            Ada.Streams.Write (This.Back.all, Item (Item'First .. Last));
            -- If no fail in the writting, mark the chunk as read:
            Circular.Skip (This.Buf_Out, Last - Item'First + 1);

            This.Total_Written := This.Total_Written + Last - Item'First + 1;
            Avg.Push (This.Avg_Out, Float (Last - Item'First + 1));
         end;
      end if;
   end Flush;

   ------------------------------------------------------------------------
   -- Get_Buffered_Count                                                 --
   ------------------------------------------------------------------------
   -- Returns the amount of written buffered data.
   function Get_Buffered_Count (This : in Stream_Type)
      return Stream_Element_Count is
   begin
      return Circular.Available_Read (This.Buf_Out);
   end Get_Buffered_Count;

   ------------------------------------------------------------------------
   -- Get_Current_Write_Rate                                             --
   ------------------------------------------------------------------------
   -- Gives the instantaneous writting speed.
   function Get_Current_Write_Rate (This : access Stream_Type) return Types.Data_Rate is
      Rate : Float;
   begin
      Avg.Average (This.Avg_Out, Rate);
      return Types.Data_Rate (Rate);
   end Get_Current_Write_Rate;

   ------------------------------------------------------------------------
   -- Get_Current_Read_Rate                                              --
   ------------------------------------------------------------------------
   -- Gives the reading speed since creation or last reset.
   function Get_Current_Read_Rate (This : access Stream_Type) return Types.Data_Rate is
      Rate : Float;
   begin
      Avg.Average (This.Avg_In, Rate);
      return Types.Data_Rate (Rate);
   end Get_Current_Read_Rate;

   ------------------------------------------------------------------------
   -- Get_Global_Write_Rate                                              --
   ------------------------------------------------------------------------
   -- Gives the writting speed since creation or last reset.
   function Get_Global_Write_Rate (This : in Stream_Type) return Types.Data_Rate is
   begin
      return
         Types.Data_Rate (This.Total_Written) /
         Types.Data_Rate (Calendar.Clock - This.Start);
   end Get_Global_Write_Rate;

   ------------------------------------------------------------------------
   -- Get_Global_Read_Rate                                               --
   ------------------------------------------------------------------------
   -- Gives the reading speed since creation or last reset.
   function Get_Global_Read_Rate (This : in Stream_Type) return Types.Data_Rate is
   begin
      return
         Types.Data_Rate (This.Total_Read) /
         Types.Data_Rate (Calendar.Clock - This.Start);
   end Get_Global_Read_Rate;

end Agpl.Streams.Bandwidth_Throttle;
