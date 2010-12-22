with Agpl.Interfaces.C.Types;

package Agpl.Boost.Graphs is

   pragma Preelaborate;

   function Inf_Weight return Interfaces.C.Types.Double;

   subtype Weight        is Interfaces.C.Types.Double;
   subtype Weight_Matrix is Interfaces.C.Types.Double_Arrays.C_Matrix;

   function Prim (Weights : Weight_Matrix)
                  return    Weight_Matrix;
   --  Non-tree edges set to Inf_Weight
   --  Tree     edges retain its weight

private

   pragma Import (C, Inf_Weight, "INF_WEIGHT");
   pragma Inline (Inf_Weight);

end Agpl.Boost.Graphs;
