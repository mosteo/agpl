--  Dirty package for inneficient solving of short distance between two points
--  in a graph.

--  Warning: for directed graph the results haven't been checked. They could
--  be reversed.

--  C code taken from en.wikipedia.com

with Interfaces.C;
with Ada.Containers.Vectors;

generic
   type Vertex_Data is private;
package Agpl.Graphs.Bellman_Ford is

   pragma Preelaborate;

   Invalid_Graph : exception;

   type Vertex_Index is new Positive;

   type Vertex is record
      Index : Vertex_Index;
      Data  : Vertex_Data;
   end record;

   type Edge is record
      Source,
      Dest   : Vertex_Index;
      Weight : Integer; -- Can be negative
   end record;

   type Edge_Array  is array  (Positive range <>) of Edge;
   type Cost_Array  is array  (Vertex_Index range <>) of Integer;
   type Cost_Matrix is array  (Vertex_Index range <>,
                               Vertex_Index range <>) of Integer;
   --  Used to represent the cost of going from any vertex to any other

   type Graph is tagged private;
   --  Graphs must be continuous: that is, vertices must be consecutively named.

   procedure Add_Edge (This : in out Graph;
                       E    : in     Edge);

   procedure Add_Undirected_Edge (This : in out Graph;
                                  E    : in     Edge);

   procedure Add_Vertex (This : in out Graph;
                         V    : in     Vertex);

   function Is_Valid (This : in Graph) return Boolean;
   --  Verify the sanity of a Graph.

   function Costs_From_Source (This   : in Graph;
                               Source : in Vertex_Index) return Cost_Array;
   --  Get the costs from a given source to any other vertex.
   --  O (Edges * Vertices)

   function Costs (This : in Graph) return Cost_Matrix;
   --  Complete costs for a graph.
   --  O (Edges * Vertices**2 )

   function Get_Edges (This : in Graph) return Edge_Array;

   function Get_Vertex (This  : in Graph;
                        Index : in Vertex_Index) return Vertex;

   function Max_Vertex (This : in Graph) return Vertex_Index;

   procedure Test_Package; -- Run a simple sanity check.
   --  Will raise Program_Error if bad (!)

private

   type C_Vertex is new Interfaces.C.Int range 0 .. Interfaces.C.Int'Last;

   type C_Edge is record
      Source,
      Dest   : C_Vertex;
      Weight : Interfaces.C.Int;
   end record;
   pragma Convention (C, C_Edge);

   type C_Edge_Array is array (Interfaces.C.Int range <>) of C_Edge;
   pragma Convention (C, C_Edge_Array);

   type C_Cost_Array is array (Interfaces.C.Int range <>) of Interfaces.C.Int;
   pragma Convention (C, C_Cost_Array);

   package C_Edge_Vectors is new Ada.Containers.Vectors (Natural, C_Edge);

   package Vertex_Vectors is new Ada.Containers.Vectors (Vertex_Index, Vertex);

   type Graph is tagged record
      C_Edges    : C_Edge_Vectors.Vector;
      Vertices   : Vertex_Vectors.Vector;
      Min_Vertex : Vertex_Index := Vertex_Index'Last;
      Max_Vertex : Vertex_Index := Vertex_Index'First;
   end record;

   procedure Bellman_Ford (Graph        : in C_Edge_Array;
                           Source       : in C_Vertex;
                           Vertex_Count : in Interfaces.C.Int;
                           Distance     :    out C_Cost_Array);
   --  For internal use

   function To_C   (This : in Vertex_Index)   return C_Vertex; pragma Inline (To_C);
   function To_Ada (This : in C_Vertex) return Vertex_Index;   pragma Inline (To_Ada);

   function Min_Vertex (This : in Graph) return Vertex_Index;

   procedure Print_C_Graph (This : in C_Edge_Array);

   function To_C_Edge_Array (This : in Graph) return C_Edge_Array;

end Agpl.Graphs.Bellman_Ford;
