with Agpl.Trace; use Agpl.Trace;

with Ada.Unchecked_Deallocation;

package body Agpl.Smart_access_Limited is

   ----------
   -- Free --
   ----------
   procedure Free is new
      Unchecked_deallocation (Item, Item_access);

   procedure Free is new
      Unchecked_deallocation (Tracker_type, Tracker_access);

   ------------------
   -- Tracker_Type --
   ------------------

   protected body Tracker_Type is

      ---------
      -- Add --
      ---------

      procedure Add (I : in Integer) is
      begin
         Count := Count + I;
      end Add;

      --------------
      -- Discount --
      --------------

      procedure Discount is
      begin
         --  Special check for finalization when uninitialized.
         if Count > 0 then
            Add (-1);
         end if;

         if Count = 0 then
            Free;  -- Could be null, no problem.
         end if;
      end Discount;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         if Debug and then Data /= null then
            Log ("Destroying last instance of value of type " & Item_Id,
                 Always, Log_Section);
         end if;
         Free (Data);
      end Free;

      ---------------
      -- Get_Count --
      ---------------

      function Get_Count return Natural is
      begin
         return Count;
      end Get_Count;

      --------------
      -- Get_Data --
      --------------

      function Get_Data return Item_Access is
      begin
         return Data;
      end Get_Data;

      -----------------
      -- Rebind_Data --
      -----------------

      procedure Rebind_Data (This : in Item_Access; Force : in Boolean) is
      begin
         if Count = 1 or else Force then
            Free;
            Data := This;
         else
            raise Allocated_Access with Item_Id;
         end if;
      end Rebind_Data;

      --------------
      -- Set_Data --
      --------------

      procedure Set_Data (This : in Item_Access) is
      begin
         if Data /= null then
            raise Allocated_Access with Item_Id;
         else
            Data  := This;
            Count := 1;
            Alloc := True;
         end if;
      end Set_Data;

      ------------
      -- Unbind --
      ------------

      procedure Unbind (Force : in Boolean) is
      begin
         if Count = 1 or else Force then

            --  The idea is that the datum lives out of our control, so we
            --  don't need to free it.
            Data  := null;
            Count := 0;
            Alloc := True; -- So deallocation could be notified in Val.

            --  Free (this.Tracker.Data);
            --  This was like that for some time after migrating from Adagio to
            --  Agpl, but I'm 99% it was wrong.
         else
            raise Allocated_Access with Item_Id;
         end if;
      end Unbind;

      ------------
      -- Adjust --
      ------------

      procedure Adjust is
      begin
         Add (1);
      exception
         when E : others =>
            Trace.Log ("Smart_Access " & Item_Id & " Adjust exception: " &
                       Trace.Report (E),
                       Trace.Error);
            --  raise; -- It's illegal to propagate exceptions from adjust?
      end Adjust;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Must_Free : out Boolean) is
      begin
         Must_Free := False;

         Discount;

         --  Race condition here?
         --  I don't think it's possible but not very thought of.
         pragma Race_Condition;
         --  Indeed is possible! Witnessed it with my own eyes!

         --  The above comments are a remainder from when this was outside of
         --  the protected body but in the main Finalize (thread-unsafe)

         Must_Free := Get_Count = 0;
      exception
         when E : others =>
            Trace.Log ("Smart_Access " & Item_Id & " Tracker.Finalize exception: " &
                       Trace.Report (E),
                       Trace.Error);
            --  raise; -- It's illegal to propagate exceptions from finalize?
      end Finalize;

   end Tracker_Type;

   -------------
   -- Is_Null --
   -------------
   --  Is null?
   function Is_Null (this : in Object) return Boolean is
   begin
      return this.Tracker.Get_Data = null;
   end Is_Null;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Object) return Boolean is
   begin
      return not This.Is_Null;
   end Is_Valid;

   ----------
   -- Bind --
   ----------
   --  Association:
   procedure Bind (this : in out Object; Data : in Item_access) is
   begin
      This.Tracker.Set_Data (Data);
      if Debug then
         Log ("Binding new value of type " & Item_Id, Always, Log_Section);
      end if;
   end Bind;

   ----------
   -- Bind --
   ----------

   function Bind (This : in Item_Access) return Object is
   begin
      return Result : Object do
         Bind (Result, This);
      end return;
   end Bind;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (This  : in out Object;
      Data  : in     Item_Access;
      Force : in     Boolean := False)
   is
   begin
      This.Tracker.Rebind_Data (Data, Force);
   end Rebind;

   ------------
   -- Unbind --
   ------------
   --  Unbinding:
   --  The value is no longer controlled
   procedure Unbind (This : in out Object; Force : in Boolean := False) is
   begin
      This.Tracker.Unbind (Force);
   end Unbind;

   ---------
   -- Ref --
   ---------

   function Ref (this : in Object) return Item_access is
      I : constant Item_Access := This.Tracker.Get_Data;
   begin
      if I = null then
         raise Constraint_Error with "Smart pointer is null";
      else
         return I;
      end if;
   end Ref;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (this : in out Object) is
   begin
      This.Tracker.Adjust;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (this : in out Object) is
      Must_Free : Boolean := False;
   begin
      if This.Tracker = null then
         raise Tracker_Already_Deallocated;
      end if;

      This.Tracker.Finalize (Must_Free);

      if Must_Free then
         Free (This.Tracker); -- Last one going out of scope.
      end if;

   exception
      when E : others =>
         Trace.Log ("Smart_Access " & Item_Id & " Finalize exception: " &
                    Trace.Report (E),
                    Trace.Error);
         --  raise; -- It's illegal to propagate exceptions from finalize?
   end Finalize;

end Agpl.Smart_Access_Limited;
