with Agpl.Trace; use Agpl.Trace;

--  with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body Agpl.Optimization.Concordefake is

   use type C.int;

   ---------------
   -- Get_Big_M --
   ---------------
   --  Compute a M big enough (sum of all other non-inf costs)
   function Get_Big_M (Cost : in Cost_Matrix) return Costs is
      Big_M : Costs := 0;
   begin
      for Row in Cost.First_Row .. Cost.Last_Row loop
         for Col in Cost.First_Col .. Cost.Last_Col loop
            --  exit when Col > Row; -- Fails with asymmetric problems!!!
            if Cost.Get (Row, Col) /= Inf and then Cost.Get (Row, Col) > 0 then
               pragma Beware ("I'm not sure this is correct, because the ATSP --> TSP transform uses -Big_M in some places. Alors?");
               --  Big_M := Big_M + abs (Cost.Get (Row, Col));

               Big_M := Costs'Max (Big_M, abs (Cost.Get (Row, Col)));
               pragma Atchung ("This is wrong for sure...");
            end if;
         end loop;
      end loop;

      Log ("Big M is" & Big_M'Img, Debug, Log_Section);

      --  return Big_M;
      return Big_M * Costs (Cost.Rows);
      pragma Danger ("The above is also suspicious...");
      --  Big_M should be the sum of all costs
      --  But when we have negative ones? -> ABS -> Too big
      --  Sum only positives --> seems to work, but gives errors in large problems
      --  Get the maximum one and make big_M = Max times Rows. Seems to work but
      --  be warned!!
   end Get_Big_M;

   ---------------
   -- Solve_TSP --
   ---------------

   function Solve_TSP
     (Start : in Start_Matrix;
      Cost  : in Cost_Matrix)
      return Result_Matrix
   is

      type Int_Cost_Array is array
        (1 .. C.Int (Cost.Rows * (Cost.Rows + 1) / 2)) of C.int;
      pragma Convention (C, Int_Cost_Array);

      type Int_Sol_Array is array
        (1 .. C.Int (Cost.Rows)) of C.int;
      pragma Convention (C, Int_Sol_Array);

--        procedure Solve (Err    :    out C.int;
--                         ncount :        C.int;
--                         costs  : in     Int_Cost_Array;
--                         argc   :        C.int;
--                         argv   : in out C.Strings.chars_ptr_array;
--                         sol    : in out Int_Sol_Array);
--        pragma Import (C, Solve, "solve_tsp_problem");
--        pragma Import_Valued_Procedure (Solve, "solve_tsp_problem");

      Min_Cost : Costs := Inf;
      New_Cost : Cost_Matrix := Cost;
   begin
      if Cost.First_Row /= 1 or else Cost.First_Col /= 1 or else
        Start'First /= 1
      then
         raise Constraint_Error;
      end if;

      pragma Assert (Cost.Rows = Cost.Cols);

      --  Print_Problem (Cost);

      --  Check symmetry and get min cost:
      for From in Cost.First_Row .. Cost.Last_Row loop
         for To in Cost.First_Col .. Cost.Last_Col loop
            exit when To > From;
            if Cost.Get (From, To) /= Cost.Get (To, From) then
               raise Constraint_Error with "TSP is not symmetric";
            end if;
            Min_Cost := Costs'Min (Min_Cost, Cost.Get (From, To));
         end loop;
      end loop;

      declare
         Co  : Int_Cost_Array;
         I   : C.Int := Co'First;
         R   : C.int;
         Sol : Int_Sol_Array;

         --  Argv  : C.Strings.chars_ptr_array := (1 .. 1 => C.Strings.Null_Ptr);
         Big_M : constant Costs := Get_Big_M (Cost);
      begin
         --  Assign values to the C matrix, moving them to the optimal range:
         --  At the same time, keep new costs in another matrix.
         for Row in Cost.First_Row .. Cost.Last_Row loop
            for Col in Cost.First_Col .. Cost.First_Col + Row - Cost.First_Row loop
               if Cost.Get (Row, Col) = Inf then
                  Co (I) := C.Int (Big_M - Min_Cost);
               else
                  Co (I) := C.int (Cost.Get (Row, Col) - Min_Cost);
               end if;
               New_Cost.Set (Row, Col, Costs (Co (I)));
               New_Cost.Set (Col, Row, New_Cost.Get (Row, Col));
               I         := I + 1;
            end loop;
         end loop;

         --  Instead of solving, we read from the given file...
         --  Solve (R, Sol'Length, Co, 0, Argv, Sol);
         declare
            F   : File_Type;
            package Int_Io is new Integer_Io (C.Int);
            use Int_Io;
         begin
            R := 0;
            Open (F, Name => +Solution_File, Mode => In_File);
            for I in Sol'Range loop
               Get (F, Sol (I));
               Put_Line ("Read:" & Sol (I)'Img);
            end loop;
            Close (F);
         end;

         if R /= 0 then
            raise No_Solution;
         else
            --  Create the solution vector, in the appropriate permutation.
            declare
               Result : Result_Matrix (1 .. 1, 1 .. Sol'Length);
               Pos    : C.Int := Sol'First;
            begin
               --  Locate the starting city:
               while Cities (Sol (Pos) + 1) /= Start (Start'First) loop
                  Pos := Pos + 1;
                  if Pos > Sol'Last then
                     raise Constraint_Error; -- Shouldn't happen
                  end if;
               end loop;

               for I in Sol'Range loop
                  Result (1,
                          Stages (I)) :=
                    Cities (Sol (Pos) + 1); -- Add one because we're 1-based

                  Pos := Pos + 1;
                  if Pos > Sol'Last then
                     Pos := Sol'First;
                  end if;
               end loop;

--                 Put_Line ("Raw solution:");
--                 for I in Sol'Range loop
--                    Put (Integer 'Image(Integer (Sol (I) + 1)));
--                 end loop;
--                 New_Line;

--                 Put_Line ("TSP solution:");
--                 Print_Solution (New_Cost, Start, Result, No_Return => False);

               return Result;
            end;
         end if;
      end;
   end Solve_TSP;

   ----------------
   -- Solve_ATSP --
   ----------------

   function Solve_ATSP (Start : in Start_Matrix;
                        Cost  : in Cost_Matrix) return Result_Matrix
   is
      Big_M    : constant Costs := Get_Big_M (Cost); -- Inf for practical purposes
      New_Cost : Cost_Matrix :=
                   Create (First_Row => Cost.First_Row,
                           Last_Row  => Cost.Last_Row * 2,
                           First_Col => Cost.First_Row,
                           Last_Col  => Cost.Last_Row * 2,
                           Default => Inf);
      --  New transformed cost matrix

      N        : constant Cities := Cities (Cost.Rows); -- Number of nodes
   begin
      if Cost.First_Row /= 1 or else Cost.First_Col /= 1 or else Start'First /= 1 then
         raise Constraint_Error;
      end if;
      pragma Assert (Cost.Rows = Cost.Cols);

      --  Check if it's symmetric...
      declare
         Asym : Boolean := False;
      begin
         Outer:
         for R in Cost.First_Row .. Cost.Last_Row loop
            for C in Cost.First_Col .. Cost.Last_Col loop
               if Cost.Get (R, C) /= Cost.Get (C, R) then
                  Asym := True;
                  exit Outer;
               end if;
            end loop;
         end loop Outer;
         if not Asym then
            Log ("Problem is symmetric, skipping transformation...",
                 Debug, Log_Section);
            return Solve_TSP (Start, Cost);
         end if;
      end;

      --  Print_Problem (Cost);

      for I in Cost.First_Row .. Cost.Last_Row loop
         New_Cost.Set (I + N, I, -Big_M);
         New_Cost.Set (I, I + N, -Big_M); -- For symmetry, but innecesary.
         --  This should be Big_M according to Reinelt.
      end loop;

      --  WARNING!! HERE WE ARE EXPLICITELY REMOVING LOOP COSTS.
      --  I THINK THIS DOESN'T AFFECT ANYTHING, BUT...
      for I in Cost.First_Row .. Cost.Last_Row loop
         for J in Cost.First_Col .. Cost.Last_Col loop
            if I /= J and then Cost.Get (I, J) /= Inf then
               New_Cost.Set (I + N, J, Cost.Get (I, J));
               New_Cost.Set (J, I + N, Cost.Get (I, J)); -- For symmetry, but innecesary.
            end if;
         end loop;
      end loop;

      --  Print_Problem (New_Cost);

      --  Get result and undo transformation:
      declare
         Sol      : constant Result_Matrix := Solve_TSP (Start, New_Cost);
         Real_Sol : Result_Matrix (1 .. 1, 1 .. Stages (N));
         Pos      : Stages := Real_Sol'First (2);
      begin
         for I in Sol'Range (2) loop
            if Sol (Sol'First, I) <= N then
               Real_Sol (Real_Sol'First, Pos) := Sol (Sol'First, I);
               Pos            := Pos + 1;
            end if;
         end loop;

         pragma Assert (Pos = Real_Sol'Last (2) + 1);

         --  Get the proper order in the symmetric solution:
         declare
            First_Sol_Cost  : constant Costs :=
                               Get_Total_Cost (Cost, Real_Sol, False);
            Second_Sol_Cost : Costs;
            Reverse_Sol     : Result_Matrix (Real_Sol'Range (1),
                                             Real_Sol'Range (2));
            Pos             : Stages := Reverse_Sol'First (2);
         begin
            --  Print_Solution (Cost, Start, Real_Sol, True);

            Reverse_Sol (Reverse_Sol'First, Pos) := Real_Sol (Real_Sol'First, 1);
            Pos := Pos + 1; -- This is to keep the first city ordering.

            for I in reverse Real_Sol'Range (2) loop
               exit when I = Real_Sol'First (2); -- Already assigned before
               Reverse_Sol (Reverse_Sol'First, Pos) := Real_Sol (Real_Sol'First, I);
               Pos := Pos + 1;
            end loop;
            pragma Assert (Pos = Reverse_Sol'Last (2) + 1);

            --  Return the proper asymmetric solution:
            Second_Sol_Cost := Get_Total_Cost (Cost, Reverse_Sol, False);
            if Costs'Min (First_Sol_Cost, Second_Sol_Cost) > Big_M then
               Log ("Straight sol cost:" & First_Sol_Cost'Img, Error);
               Log ("Reverse  sol cost:" & Second_Sol_Cost'Img, Error);
               Log ("Big_M:" & Big_M'Img, Error);
               raise No_Solution; -- Forbidden link used!
            end if;

            --  Check that the costs in the solutions match!!
            pragma Omitted_Check_By_Fakeness;
--              if Get_Total_Cost (New_Cost, Sol, No_Return => False) +
--                (Big_M * Costs (N)) /=
--                Costs'Min (First_Sol_Cost, Second_Sol_Cost) then
--                 raise Program_Error; -- Solution isn't optimal, some error happened.
--              end if;

            Log ("Cost TSP :" & Get_Total_Cost (New_Cost, Sol, False)'Img,
                 Debug, Log_Section);
            Log ("Cost ATSP: " & Costs'Min (First_Sol_Cost, Second_Sol_Cost)'Img,
                 Debug, Log_Section);

            if Second_Sol_Cost < First_Sol_Cost then
--                 Put_Line ("ATSP solution");
--                 for I in Reverse_Sol'Range (2) loop
--                    Put (Reverse_Sol (1, I)'Img);
--                 end loop;
--                 New_Line;
               return Reverse_Sol;
            else
--                 Put_Line ("ATSP solution");
--                 for I in Real_Sol'Range (2) loop
--                    Put (Real_Sol (1, I)'Img);
--                 end loop;
--                 New_Line;
               return Real_Sol;
            end if;
         end;
      end;
   end Solve_ATSP;

   ----------------
   -- Solve_MTSP --
   ----------------

   function Solve_MTSP (Start     : in Start_Matrix;
                        Cost      : in Cost_Matrix;
                        No_Return : in Boolean := False) return Result_Matrix
   is
      N        : constant Cities   := Cities (Cost.Rows); -- Number of nodes
      M        : constant Salesmen := Start'Length;    -- Number of travelers

      New_Cost : Cost_Matrix :=
                   Create (First_Row => Cost.First_Row,
                           Last_Row  => Cost.First_Row + N + Cities (M) - 1,
                           First_Col => Cost.First_Col,
                           Last_Col  => Cost.First_Col + N + Cities (M) - 1,
                           Default   => Inf);
   begin
      if Cost.First_Row /= 1 or else Cost.First_Col /= 1 or else Start'First /= 1 then
         raise Constraint_Error;
      end if;
      pragma Assert (Cost.Rows = Cost.Cols);

      --  If just a salesman, don't even do the transform:
      if Start'Length = 1 and then not No_Return then
         Log ("Problem is single salesman, skipping transformation...",
              Debug, Log_Section);
         return Solve_ATSP (Start, Cost); -- ----- EARLY EXIT POINT
      end if;

      --  If No_Return, special case:
      if No_Return then
         return Solve_MTSP_No_Return (Start, Cost);
      end if;

      --  Keep original costs:
      for Row in Cost.First_Row .. Cost.Last_Row loop
         for Col in Cost.First_Col .. Cost.Last_Col loop
            New_Cost.Set (Row, Col, Cost.Get (Row, Col));
         end loop;
      end loop;

      --  Artificial cities costs
      for Row in New_Cost.First_Row .. Cost.Last_Row loop
         for Col in New_Cost.First_Col .. Cost.Last_Col loop
            if Row > Cost.Last_Row and then Col = Row + 1 then
               --  From artificial i to i + 1
               --  New_Cost.Get (Row, Col) := 0;
               null; -- I deviate here from Helmberg suggestion; I prefer that
                     --  each traveller explicitely visits its own city.
            elsif Row = New_Cost.Last_Row and then Col = Cost.Last_Col + 1 then
               --  From artificial i_m to i
               --  New_Cost.Get (Row, Col) := 0;
               null; -- I deviate here from Helmberg suggestion; I prefer that
                     --  each traveller explicitely visits its own city.
            elsif Row > Cost.Last_Row and then Col <= Cost.Last_Col then
               --  From artificial to real
               declare
                  Salesman : constant Salesmen range 1 .. M := Salesmen (Row - N);
                  Starting : constant Cities   range 1 .. N := Start (Start'First + Salesman - 1);
               begin
                  if Col = Starting then
                     New_Cost.Set (Row, Col, 0); -- Already there
                  else
                     New_Cost.Set (Row, Col, Cost.Get (Starting, Col));
                  end if;
               end;
            elsif Row <= Cost.Last_Row and then Col > Cost.Last_Col then
               --  From real to artificial
               declare
                  Salesman : Salesmen range 1 .. M := Salesmen (Col - N);
                  Starting : Cities   range 1 .. N;
               begin
                  --  Displace salesman index, since now false city i + 1 correspond to salesman i
                  if Salesman = 1 then
                     Salesman := M;
                  else
                     Salesman := Salesman - 1;
                  end if;
                  --  And set the starting city for this salesman:
                  Starting := Start (Start'First + Salesman - 1);

                  if Row = Starting then
                     New_Cost.Set (Row, Col, 0); -- Already there
                  else
                     New_Cost.Set (Row, Col, Cost.Get (Row, Starting));
                  end if;
               end;
            elsif Row > Cost.Last_Row and then Col > Cost.Last_Col then
               --  From artificial to artificial: already at Infinite cost.
               null;
            elsif Row <= Cost.Last_Row and then Col <= Cost.Last_Col then
               --  From real to real, already assigned before
               null;
            else
               raise Program_Error; -- Something have we forgotten!
            end if;
         end loop;
      end loop;

      --  DEBUG: cost for transformed problem:
      --  Put_Line ("----------------------------");
      --  Print_Problem (New_Cost);
      --  Put_Line ("----------------------------");

      --  Get ATSP solution:
      declare
         ATSP_Sol : constant Result_Matrix := Solve_ATSP ((1 .. 1 => N + 1),
                                                          New_Cost);
         Artif    : Cities range 1 .. N + Cities (M);
      begin
         --  We have now a solution that shall have N + 1, N + 2 .. N + M cities
         --  visited in that order. If not, something is wrong!
         --  COULD BE TRUE WHEN JUST 2 SALESMEN?? SHOULD WE ORDER IT IN THAT CASE!!
         --  Verification:
         Artif := ATSP_Sol (ATSP_Sol'First, ATSP_Sol'First (2));
         pragma Assert (Artif = N + 1);
         for I in ATSP_Sol'First (2) + 1 .. ATSP_Sol'Last (2) loop
            if ATSP_Sol (ATSP_Sol'First, I) > N then
               if ATSP_Sol (ATSP_Sol'First, I) /= Artif + 1 then
                  raise Program_Error;
               else
                  Artif := Artif + 1;
               end if;
            end if;
         end loop;

         --  DEBUG: ATSP transformation result:
         Print_Solution (New_Cost, (1 .. 1 => N + 1), ATSP_Sol, No_Return);

         --  Get the MTSP solution:
         declare
            Salesman  : Salesmen range 1 .. M;
            Sol       : Result_Matrix (1 .. M, 1 .. Stages (N));
            Pos       : Stages range Sol'First (2) .. Sol'Last (2) + 1;
         begin
            for I in ATSP_Sol'Range (2) loop
               --  New traveller?
               if ATSP_Sol (ATSP_Sol'First, I) > N then

                  --  Prepare everything for a new salesman:
                  Salesman  := Salesmen (ATSP_Sol (ATSP_Sol'First, I) - N);
                  Pos       := Sol'First (2);

                  --  Fill its solution with its starting/final city:
                  for J in Sol'Range (2) loop
                     Sol (Salesman, J) := Start (Salesman);
                  end loop;

               else
                  --  Regular city, add to current salesman tour!
                  Sol (Salesman, Pos) := ATSP_Sol (ATSP_Sol'First, I);
                  Pos       := Pos + 1;
               end if;
            end loop;

            return Sol;
         end;
      end;
   end Solve_MTSP;

   --------------------------
   -- Solve_MTSP_No_Return --
   --------------------------
   --  When No_Return, the transformation is other: we no longer need the
   --  "signaling start" false cities. We simply set all the return to a base
   --  costs to zero, and when a new base is visited this is the start of the
   --  agent placed there.
   function Solve_MTSP_No_Return (Start     : in Start_Matrix;
                                  Cost      : in Cost_Matrix) return Result_Matrix
   is
      Mod_Cost : Cost_Matrix := Cost;

      -------------
      -- Is_Base --
      -------------

      function Is_Base (X : in Cities) return Boolean is
      begin
         for I in Start'Range loop
            if Start (I) = X then
               return True;
            end if;
         end loop;
         return False;
      end Is_Base;

      -----------------------
      -- Salesman_For_Base --
      -----------------------

      function Salesman_For_Base (X : in Cities) return Salesmen is
      begin
         pragma Assert (Is_Base (X), "No base of any salesman");
         for I in Start'Range loop
            if Start (I) = X then
               return I;
            end if;
         end loop;

         raise Program_Error;
      end Salesman_For_Base;

      --  Big_M : constant Costs := Get_Big_M (Cost);
   begin
      pragma Assert (Mod_Cost.First_Row = 1 and then Mod_Cost.First_Col = 1);
      Log ("Doing No_Return transformation...", Debug, Log_Section);

      --  Nullify costs of returning to base
      for From in Mod_Cost.First_Row .. Cost.Last_Row loop
         for To in Mod_Cost.First_Col .. Cost.Last_Col loop
            if Is_Base (To) then
               Mod_Cost.Set (From, To, 0);
            end if;
         end loop;
      end loop;

      --  Print_Problem (Mod_Cost);

      declare
         aSol : constant Result_Matrix :=
                  Solve_ATSP ((1 => Start (Start'First)), Mod_Cost);
         Sol  :          Result_Matrix (1 .. Start'Length, 1 .. Stages (Cost.Rows));

         Salesman : Salesmen := Sol'First;
         Stage    : Stages   := Sol'First (2) + 1;
      begin
         --  Fill solution with starting/final cities:
         for Man in Sol'Range loop
            for J in Sol'Range (2) loop
               Sol (Man, J) := Start (Man);
            end loop;
         end loop;

         --  Unfold transformation
         for Pos in aSol'Range (2)loop
            if Is_Base (aSol (1, Pos)) then
               --  Switch to Salesman starting there:
               Salesman := Salesman_For_Base (aSol (1, Pos));
               Stage    := Sol'First (2) + 1;
            else
               --  Assign this city to current salesman:
               Sol (Salesman, Stage) := aSol (1, Pos);
               Stage := Stage + 1;
            end if;
         end loop;

         return Sol;
      end;
   end Solve_MTSP_No_Return;

   --------------------
   -- Get_Total_Cost --
   --------------------

   function Get_Total_Cost
     (Cost  : in Cost_Matrix;
      Sol   : in Result_Matrix;
      No_Return : in Boolean)
      return Costs
   is
      Total : Costs := 0;
   begin
      if Sol'Length = 1 and then
        Normalize_Tour (Sol (1, 1), Sol)'Length (2) = Sol'Length (2) then
         --  It is a proper normalized single solution:

         for City in Sol'First (2) + 1 .. Sol'Last (2) loop
            begin
               Total := Total + Cost.Get (Sol (1, City - 1),
                                      Sol (1, City));
            exception
               when Constraint_Error =>
                  return Inf;
            end;
         end loop;

         if not No_Return then
            begin
               Total := Total + Cost.Get (Sol (1, Sol'Last (2)),
                                      Sol (1, Sol'First (2)));
            exception
               when Constraint_Error =>
                  return Inf;
            end;
         end if;

      else
         --  Compound or unnormalized tour:
         for Man in Sol'Range loop
            Total := Total + Get_Total_Cost (Cost,
                                             Normalize_Tour (Sol (Man, 1),
                                                             Sol),
                                             No_Return);
         end loop;
      end if;

      return Total;
   end Get_Total_Cost;

   ----------------------
   -- Get_Max_Min_Cost --
   ----------------------

   function Get_Min_Max_Cost
     (Cost  : in Cost_Matrix;
      Sol   : in Result_Matrix;
      No_Return : in Boolean)
      return Costs
   is
      Worst : Costs := Costs'First;
   begin
      for Salesman in Sol'Range (1) loop
         declare
            --  Construct a solution involving just a salesman
            Single_Sol : Result_Matrix (1 .. 1, Sol'Range (2));
         begin
            for I in Sol'Range (2) loop
               Single_Sol (1, I) := Sol (Salesman, I);
            end loop;
            Worst := Costs'Max (Worst, Get_Total_Cost (Cost, Single_Sol, No_Return));
         end;
      end loop;

      return Worst;
   end Get_Min_Max_Cost;

   --------------------
   -- Normalize_Tour --
   --------------------

   function Normalize_Tour (Start : in Cities;
                            Sol   : in Result_Matrix) return Result_Matrix
   is
      Man : Salesmen;
      Pos : Stages;
      Ok  : Boolean := False;
   begin
      --  Locate which salesman is the one with that starting city:
      --  And where his starting city is:
      for Men in Sol'Range loop
         if Sol (Men, Sol'First (2)) = Start or else Sol (Men, Sol'Last (2)) = Start then
            Man := Men;
            Ok  := True;
            exit;
         end if;
      end loop;

      if not Ok then
         raise Program_Error;
      end if;

      if Sol (Man, Sol'First (2)) = Start then
         Pos := Sol'First (2) + 1;
      else
         Pos := Sol'First (2);
      end if;

      declare
         New_Sol : Result_Matrix (1 .. 1, 1 .. Sol'Length (2) + 1);
         Last    : Stages;
      begin
         for I in New_Sol'Range (2) loop
            New_Sol (1, I) := Start;
         end loop;
         for Stage in New_Sol'First (2) + 1 .. New_Sol'Last (2) - 1 loop
            exit when (Sol (Man, Pos) = Start and then not
                         (Pos < Sol'Last (2) and then Sol (Man, Pos + 1) /= Start))
              or else Pos > Sol'Last (2);
            New_Sol (1, Stage) := Sol (Man, Pos);
            Pos := Pos + 1;
         end loop;

         for I in New_Sol'First (2) + 1 .. New_Sol'Last (2) loop
            if New_Sol (1, I) = Start then
               Last := I;
               exit;
            end if;
         end loop;

         declare
            Good_Sol : Result_Matrix (1 .. 1, 1 .. Last - 1);
         begin
            for J in Good_Sol'Range (2) loop
               Good_Sol (1, J) := New_Sol (1, J);
            end loop;
            return Good_Sol;
         end;
      end;
   end Normalize_Tour;

   -------------------
   -- Print_Problem --
   -------------------

   procedure Print_Problem (Cost : in Cost_Matrix)
   is
      package Int_Io is new Integer_Io (Costs);
      use Int_Io;
   begin
      Int_Io.Default_Width := 5;
      New_Line;
      for I in Cost.First_Row .. Cost.Last_Row loop
         for J in Cost.First_Col .. Cost.Last_Col loop
            if Cost.Get (I, J) = Inf then
               Put ("  XXX");
            else
               Put (Cost.Get (I, J));
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Problem;

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (Cost : in Cost_Matrix;
                             Start: in Start_Matrix;
                             Sol   : in Result_Matrix;
                             No_Return : in Boolean)
   is
   begin
      for Salesman in Sol'Range (1) loop
         Put_Line ("Salesman" & Salesman'Img);
         declare
            Tour : constant Result_Matrix := Normalize_Tour (Start (Salesman), Sol);
         begin
            for City in Tour'Range (2) loop
               Put (Tour (1, City)'Img);
            end loop;
            Put_Line (" (" & Get_Total_Cost (Cost, Tour, No_Return)'Img & ")");
         end;
      end loop;

      Put_Line ("Total cost:   " & Get_Total_Cost (Cost, Sol, No_Return)'Img);
      Put_Line ("Min-max cost: " & Get_Min_Max_Cost (Cost, Sol, No_Return)'Img);
   end Print_Solution;

   ------------
   -- Create --
   ------------

   function Create (Num_Men : in Salesmen) return Normal_Tour is
      Empty  : City_Vector;
      Result : Normal_Tour;
   begin
      for I in 1 .. Num_Men loop
         Result.Tours.Append (Empty);
      end loop;

      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Start : in Start_Matrix;
                    Sol   : in Result_Matrix) return Normal_Tour
   is
      Result : Normal_Tour;
   begin
      for I in Start'Range loop
         declare
            Tour : constant Result_Matrix := Normalize_Tour (Start (I), Sol);
            Vec  : City_Vector;
         begin
            for J in Tour'Range (2) loop
               Vec.Append (Tour (Tour'First, J));
            end loop;
            Result.Tours.Append (Vec);
         end;
      end loop;

      return Result;
   end Create;

   -----------------
   -- Append_City --
   -----------------

   procedure Append_City (This : in out Normal_Tour;
                          Man  : in     Salesmen;
                          City : in     Cities)
   is
   begin
      This.Tours.Vector (Integer (Man)).Append (City);
   end Append_City;

   ----------
   -- City --
   ----------

   function City (This  : in Normal_Tour;
                  Man   : in Salesmen;
                  Stage : in Stages) return Cities
   is
   begin
      return This.Tours.Vector (Integer (Man)).Vector (Integer (Stage));
   end City;

   ----------
   -- Last --
   ----------

   function Last (This : in Normal_Tour) return Salesmen
   is
   begin
      return Salesmen (This.Tours.Last);
   end Last;

   ----------
   -- Last --
   ----------

   function Last (This : in Normal_Tour;
                  Man  : in Salesmen) return Stages
   is
   begin
      return Stages (This.Tours.Vector (Integer (Man)).Last);
   end Last;

end Agpl.Optimization.Concordefake;
