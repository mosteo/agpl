package body Agpl.Boost.Graphs is

   ----------
   -- Prim --
   ----------

   function Prim
     (Weights : Interfaces.C.Types.Double_Arrays.C_Matrix)
      return Interfaces.C.Types.Double_Arrays.C_Matrix
   is
      use Interfaces.C.Types;

      procedure Prim_C (W : access Double;
                        V :        Int;
                        T : access Double);
      pragma Import (C, Prim_C, "agpl__boost__minimum_spanning_tree");

      Result : Double_Arrays.C_Matrix (Weights'Range, Weights'Range);
   begin
      Prim_C (Weights (Weights'First (1), Weights'First (2))'Unrestricted_Access,
              Weights'Length (1),
              Result (Result'First (1),   Result'First (2))'Access);
      return Result;
   end Prim;

end Agpl.Boost.Graphs;
