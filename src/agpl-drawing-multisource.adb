--  with Agpl.Trace; use Agpl.Trace;

package body Agpl.Drawing.Multisource is

   protected body Safe_Type is

   ----------
   -- Draw --
   ----------

      procedure Draw
        (Id   :        String;
         What :        Drawable'Class)
      is
      begin
         Dirty := True;
         if What in Buffer.Object'Class then
            Buffers.Include (Id, Buffer.Object'Class (What));
         else
            declare
               Buff : Buffer.Object;
            begin
               What.Draw (Buff);
               Buffers.Include (Id, Buff);
            end;
         end if;
      end Draw;

      -----------
      -- Flush --
      -----------

      procedure Flush
        (Dest : in out Drawer'Class)
      is
         use Id_Buffer_Maps;
         procedure Flush (I : Cursor) is
            procedure Flush (K : String; B : Buffer.Object'Class) is
               pragma Unreferenced (K);
            begin
               B.Draw (Dest);
            end Flush;
         begin
            Query_Element (I, Flush'Access);
         end Flush;
      begin
         Buffers.Iterate (Flush'Access);
         Dirty := False;
      end Flush;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Dirty := True;
         Buffers.Clear;
      end Clear;

      -----------
      -- Clear --
      -----------

      procedure Clear
        (Id   :        String)
      is
      begin
         Dirty := True;
         Buffers.Exclude (Id);
      end Clear;

      -----------
      -- Dirty --
      -----------

      function Is_Dirty return Boolean is
      begin
         return Safe_Type.Dirty;
      end Is_Dirty;

   end Safe_Type;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This : in out Object;
      Id   :        String;
      What :        Drawable'Class)
   is
   begin
      This.Safe.Draw (Id, What);
   end Draw;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (This :        Object;
      Dest : in out Drawer'Class)
   is
   begin
      This.Self.Safe.Flush (Dest);
   end Flush;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      This.Safe.Clear;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (This : in out Object;
      Id   :        String)
   is
   begin
      This.safe.Clear (Id);
   end Clear;

   -----------
   -- Dirty --
   -----------

   function Is_Dirty (This : Object) return Boolean is
   begin
      return This.Safe.Is_Dirty;
   end Is_Dirty;

end Agpl.Drawing.Multisource;
