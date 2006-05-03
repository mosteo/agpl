package body Agpl.Random is

   Internal : Object;

   ------------------
   -- Open_Uniform --
   ------------------

   function Open_Uniform return Open_Uniformly_Distributed is
   begin
      return Internal.Open_Uniform;
   end Open_Uniform;

   -------------
   -- Uniform --
   -------------

   function Uniform return Uniformly_Distributed is
   begin
      return Internal.Uniform;
   end Uniform;

   ----------------------
   -- Uniform_Discrete --
   ----------------------

   function Uniform_Discrete return Discrete is
      Min : constant Integer := Discrete'Pos (Discrete'First);
      Max : constant Integer := Discrete'Pos (Discrete'Last);
   begin
      return Discrete'Val (Get_Integer (Min, Max));
   end Uniform_Discrete;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Min, Max : in Integer) return Integer is
   begin
      return Internal.Get_Integer (Min, Max);
   end Get_Integer;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Object) is
   begin
      Ada.Numerics.Float_Random.Reset (This.Gen);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Object; Initiator : in Integer) is
   begin
      Ada.Numerics.Float_Random.Reset (This.Gen, Initiator);
   end Reset;

   -------------
   -- Uniform --
   -------------

   function Uniform (This : in Object) return Uniformly_Distributed is
   begin
      return Ada.Numerics.Float_Random.Random (This.Gen);
   end Uniform;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (This     : in Object;
      Min, Max : in Integer)
      return Integer
   is
   begin
      if Max < Min then
         return Max;
      else
         return Integer'Min (Min + Integer (Float'Floor (This.Uniform *
                                                           Float (Max - Min + 1))),
                             Max);
      end if;
   end Get_Integer;

   ---------------------
   -- Discrete_Random --
   ---------------------

   function Discrete_Random (This : in Object) return Discrete is
      Min : constant Integer := Discrete'Pos (Discrete'First);
      Max : constant Integer := Discrete'Pos (Discrete'Last);
   begin
      return Discrete'Val (Get_Integer (Min, Max));
   end Discrete_Random;

   ------------------
   -- Open_Uniform --
   ------------------

   function Open_Uniform (This : in Object) return Open_Uniformly_Distributed is
   begin
      return Float'Min (Ada.Numerics.Float_Random.Random (This.Gen),
                        Open_Uniformly_Distributed'Last);
   end Open_Uniform;

begin
   Internal.Reset;
end Agpl.Random;
