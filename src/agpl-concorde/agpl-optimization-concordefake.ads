 

--  Binding to the C concorde TSP solver.
--  With transformations ATSP -> TSP, mATSP -> TSP

with Agpl.Containers.Naked_Vectors;
with Agpl.Generic_Dense_Matrix;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Interfaces.C;

--  Attempt at recovering the solution from a concorde .sol file

package Agpl.Optimization.Concordefake is

   pragma Elaborate_Body;

   Solution_File : Ustring;
   --  Fill this in with the desired file to attempt!!

   package C renames Interfaces.C;

   Log_Section : constant String := "concorde";

   No_Solution : exception;

   type Costs is new C.int; -- Costs must be given as integers.

   Inf : constant Costs;
   --  Use this value for invalid routes.

   type Salesmen is new Positive;
   type Cities   is new Positive;
   type Stages   is new Positive;

   package Cost_Matrices is new Generic_Dense_Matrix (Cities, Costs);
   subtype Cost_Matrix is Cost_Matrices.Matrix; use Cost_Matrices;
   --  Indexes are from-city, to-city
   --  For symmetric problems, just the lower half of the matrix must be filled.

   type Result_Matrix is array (Salesmen range <>,
                                Stages range <>) of Cities;
   --  First index is the salesman index.
   --  Second index is the stage index.
   --  The value says which city is visited in each stage.
   --  When no more moves, the same city will be reported.

   type Start_Matrix is array (Salesmen range <>) of Cities;
   --  Index is the salesman index.
   --  Value is in which city it starts.

   function Solve_TSP (Start : in Start_Matrix;
                       Cost  : in Cost_Matrix) return Result_Matrix;
   --  May raise No_Solution.
   --  Just the lower half of Cost will be considered.

   function Solve_ATSP (Start : in Start_Matrix;
                        Cost  : in Cost_Matrix) return Result_Matrix;
   --  May raise No_Solution.
   --  Do the standard transformation ATSP -> TSP and solve.

   function Solve_MTSP (Start     : in Start_Matrix;
                        Cost      : in Cost_Matrix;
                        No_Return : in Boolean := False) return Result_Matrix;
   --  May raise No_Solution;
   --  Do the Helmberg MTSP -> ATSP transformation and solve.
   --  If No_Return, cost of each traveler coming to base isn't used during solving.
   --  Bear in mind that each agent has to have its own starting city (no single depot).

   function Get_Total_Cost (Cost  : in Cost_Matrix;
                            Sol   : in Result_Matrix;
                            No_Return : in Boolean) return Costs;
   --  Total cost incurred by all salesmen.

   function Get_Min_Max_Cost (Cost  : in Cost_Matrix;
                              Sol   : in Result_Matrix;
                              No_Return : in Boolean) return Costs;
   --  Cost of the worst salesman.

   function Normalize_Tour (Start : in Cities;
                            Sol   : in Result_Matrix) return Result_Matrix;
   --  Will extract the tour of a single salesman, whose start is at Start.
   --  The resulting matrix will have only a single salesman index = 1.
   --  The lenght of its tour will be clipped to the actual cities visited.
   --  First city will always be Start, last city the last one visited.

   type Normal_Tour is tagged private;
   --  A normalized store for mtsp tour solutions.
   --  The starting city for every man is at index 1.

   function Create (Num_Men : in Salesmen) return Normal_Tour;
   --  Empty Normal_Tour for Num_Men travelers.

   function Create (Start : in Start_Matrix;
                    Sol   : in Result_Matrix) return Normal_Tour;
   --  Get a complete solution and return a normalized one.

   procedure Append_City (This : in out Normal_Tour;
                          Man  : in     Salesmen;
                          City : in     Cities);
   --  Add City to the end of Man's tour.

   function City (This  : in Normal_Tour;
                  Man   : in Salesmen;
                  Stage : in Stages) return Cities;
   --  Get city for Salesman at given Stage.

   function Last (This : in Normal_Tour) return Salesmen;
   --  Get last salesman.

   function Last (This : in Normal_Tour;
                  Man  : in Salesmen) return Stages;
   --  Get last stage for some salesman.

   procedure Print_Problem (Cost : in Cost_Matrix);

   procedure Print_Solution (Cost : in Cost_Matrix;
                             Start: in Start_Matrix;
                             Sol   : in Result_Matrix;
                             No_Return : in Boolean);
   --  To stdout

private

   Inf : constant Costs := Costs'Last;

   package Cities_Vector is new Agpl.Containers.Naked_Vectors (Cities);
   subtype City_Vector is Cities_Vector.Object (First => 1);

   package Tour_Vector   is new Agpl.Containers.Naked_Vectors (City_Vector);

   type Normal_Tour is tagged record
      Tours : Tour_Vector.Object (First => 1);
   end record;
   --  That's is, an array of arrays of cities.
   --  First array is indexed by salesman, second one by stage.

   function Solve_MTSP_No_Return (Start     : in Start_Matrix;
                                  Cost      : in Cost_Matrix) return Result_Matrix;
   --  Used internally to do the No_Return special case.

end Agpl.Optimization.Concordefake;
