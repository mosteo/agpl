with Agpl.Strings;
with Agpl.Trace;    use Agpl.Trace;
with Agpl.Ustrings; use Agpl.Ustrings;

package body Agpl.Gdk is

   ---------
   -- "*" --
   ---------

   function "*" (M : Float_Matrix; V : Float_Vector) return Float_Vector is
      L : Float_Vector renames V;
   begin
      return
        (L (1) * M (1, 1) + L (2) * M (1, 2) + L (3) * M (1, 3),
         L (1) * M (2, 1) + L (2) * M (2, 2) + L (3) * M (2, 3),
         L (1) * M (3, 1) + L (2) * M (3, 2) + L (3) * M (3, 3));
   end "*";

   -------------------
   -- Cross_Product --
   -------------------

   function "**" (L, R : Float_Vector) return Float_Vector is
   begin
      return
        (L (2) * R (3) - L (3) * R (2),
         L (3) * R (1) - L (1) * R (3),
         L (1) * R (2) - L (2) * R (1));
   end "**";

   -------
   -- H --
   -------

   function H (X : Float_Vector) return Float_Vector is
      Point : Float_Vector renames X;
   begin
      if Point (3) = 0.0 or else Point (3) = 1.0 then
         return Point;
      else
         return (Point (1) / Point (3), Point (2) / Point (3), 1.0);
      end if;
   end H;

--     -------------
--     -- Inverse --
--     -------------
--
--     function Inverse_Old (X : Float_Matrix) return Float_Matrix is
--        B                        : Float_Matrix;
--        Result                   : Float_Matrix;
--        I_PIVOT, J_PIVOT         : Integer range 1 .. X'Length (1);
--        BIG_ENTRY, TEMP, EPSILON : Float := 0.0;
--        L, M                     : array (1 .. X'Length (1)) of Integer;
--
--     --  *******************************************************************
--     --   This function performs the square Float_Matrix operation of "
--     --  Float_Matrix A
--     --   raise to Integer power P ".  When  P is negative , say P = -N ,
--     --   A**(-N) = (inverse(A))**N , that is, the inverse of A raise to
--     --   power N .  In this case, Float_Matrix A must be non-singular.
--     --   Exceptions will be raised if the Float_Matrix A is not a square
--     --  Float_Matrix,
--     --   or if Float_Matrix A is singular.
--     --  *******************************************************************
--     begin
--        if X'Length (1) /= X'Length (2) then
--           raise Constraint_Error;
--        end if;
--
--        B := X;
--
--        --  initiate the Row and Column interchange information
--
--        for K in  B'Range (1) loop
--           L (K) := K; -- Row interchage information
--           M (K) := K; -- Column interchange information
--        end loop;
--
--        --  major loop for inverse
--
--        for K in  B'Range (1) loop
--
--           --  & search for Row and Column Integer I_PIVOT, J_PIVOT
--           --  & both in (K .. B'LAST(1) ) for maximum B(I,J)
--           --  & in absolute value :BIG_ENTRY
--
--           BIG_ENTRY := 0.0;
--           --
--           --  check Float_Matrix singularity
--           --
--           for I in  K .. B'Last (1) loop
--              for J in  K .. B'Last (1) loop
--                 if abs (B (I, J)) > abs (BIG_ENTRY) then
--                    BIG_ENTRY := B (I, J);
--                    I_PIVOT   := I;
--                    J_PIVOT   := J;
--                 end if;
--              end loop;
--           end loop;
--           if K = B'First (1) then
--              if BIG_ENTRY = 0.0 then
--                 raise Constraint_Error with "Matrix singular";
--              else
--                 EPSILON := Float (B'Length (1)) *
--                            abs (BIG_ENTRY) *
--                            0.000001;
--              end if;
--           else
--              if abs (BIG_ENTRY) < EPSILON then
--                 raise Constraint_Error with "Matrix singular";
--              end if;
--           end if;
--
--           --  interchange Row and Column
--
--           --& interchange K-th and I_PIVOT-th Rows
--           if I_PIVOT /= K then
--              for J in  B'Range (1) loop
--                 TEMP           := B (I_PIVOT, J);
--                 B (I_PIVOT, J) := B (K, J);
--                 B (K, J)       := TEMP;
--              end loop;
--              L (K) := I_PIVOT;
--           end if;
--           --& interchange K-th and J_PIVOT-th Columns
--           if J_PIVOT /= K then
--              for I in  B'Range (1) loop
--                 TEMP           := B (I, J_PIVOT);
--                 B (I, J_PIVOT) := B (I, K);
--                 B (I, K)       := TEMP;
--              end loop;
--              M (K) := J_PIVOT;
--           end if;
--
--           --& divide K-th Column by minus pivot (-BIG_ENTRY)
--
--           for I in  B'Range (1) loop
--              if I /= K then
--                 B (I, K) := B (I, K) / (-BIG_ENTRY);
--              end if;
--           end loop;
--
--           --  reduce Float_Matrix Row by Row
--
--           for I in  B'Range (1) loop
--              if I /= K then
--                 for J in  B'Range (1) loop
--                    if J /= K then
--                       B (I, J) := B (I, J) + B (I, K) * B (K, J);
--                    end if;
--                 end loop;
--              end if;
--           end loop;
--
--           --& divide K-th Row by pivot
--
--           for J in  B'Range (1) loop
--              if J /= K then
--                 B (K, J) := B (K, J) / BIG_ENTRY;
--              end if;
--           end loop;
--           B (K, K) := 1.0 / BIG_ENTRY;
--
--        end loop; -- end of major inverse loop
--
--        --  final Column and Row interchange to obtain
--        --  inverse of X, i.e. X**(-1)
--
--        for K in reverse  B'Range (1) loop
--           --  Column interchage
--           J_PIVOT := L (K);
--           if J_PIVOT /= K then
--              --& intechange B(I,J_PIVOT) and B(I,K) for each Row I
--              for I in  B'Range (1) loop
--                 TEMP           := B (I, J_PIVOT);
--                 B (I, J_PIVOT) := B (I, K);
--                 B (I, K)       := TEMP;
--              end loop;
--           end if;
--           --  Row interchage
--           I_PIVOT := M (K);
--           if I_PIVOT /= K then
--              --& INTECHANGE B(I_PIVOT,J) and B(K,J) for each Column J
--              for J in  B'Range (1) loop
--                 TEMP           := B (I_PIVOT, J);
--                 B (I_PIVOT, J) := B (K, J);
--                 B (K, J)       := TEMP;
--              end loop;
--           end if;
--        end loop;
--
--        --  inverse of X is obtained and stored in B
--
--        Result := B;
--
--        return Result;
--     end Inverse_Old;

   -----------
   -- Print --
   -----------

   procedure Print (X : Float_Matrix) is
   begin
      for R in X'Range loop
         declare
            Line : UString;
         begin
            for C in X'Range (2) loop
               ASU.Append
                 (Line,
                  Strings.Rpad (Strings.To_string (X (R, C), 10), 20));
            end loop;
            Log ("Matrix row" & R'Img & ":" & S (Line), Always, Log_Section);
         end;
      end loop;
   end Print;

end Agpl.Gdk;
