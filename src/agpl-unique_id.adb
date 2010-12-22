package body Agpl.Unique_Id is

   -----------
   -- Value --
   -----------

   function Value (This : String) return Object is
   begin
      return (Id => +This);
   end Value;

   -----------
   -- Image --
   -----------

   function Image (This : Object) return String is
   begin
      return +This.Id;
   end Image;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Object; Id : String) is
   begin
      This := Value (Id);
   end Set;

end Agpl.Unique_Id;
