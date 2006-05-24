------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

--  The difference with Expres.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Cr.Assignment;
with Agpl.Cr.Cost_Matrix;
with Agpl.Htn.Plan;
with Agpl.Optimization.Annealing;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Agpl.Cr.Mutable_Assignment is

   type Object is tagged private;
   --  No longer is necessary for it to be lightweight, since it's only
   --  copied when a better solution is found, and this doesn't happen
   --  a lot of times. We'll try, however.

   --  Copying should be O(n)

   type Undo_Info is private;

   subtype Cost is Optimization.Annealing.Cost;

   ---------------------------
   -- ANNEALING SUBPROGRAMS --
   ---------------------------

   function Evaluate_Minimax (This : in Object) return Cost;
   --  Return Inf if invalid

   function Evaluate_Totalsum (This : in Object) return Cost;
   --  Return Inf if invalid

   procedure Mutate (This : in out Object);
   --  The result can be invalid if restrictions are violated

   function Last_Mutation (This : in Object) return String;
   --  Informative, to know mutations working well
   --  Just returns a description of what was done.

   procedure Undo (This : in out Object);
   --  Must undo the last mutation. Only one level of undo is required.

   ----------------------------
   -- MANAGEMENT SUBPROGRAMS --
   ----------------------------

   procedure Remove_Agent (This : in out Object; Name : in String);
   --  Cut out this agent; its tasks will go to others

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cr.Cost_Matrix.Object);
   --  Any task not present here will be assumed is not doable.
   --  Setting the costs, automatically add these agents as alive.

   procedure Set_Tasks (This : in out Object;
                        Plan : in     Htn.Plan.Object);
   --  The tasks are provided in Plan form, inflated or not.

   --------------
   -- MUTATING --
   --------------

   procedure Add_Mutation (This    : in out Object;
                           Mutator : not null access
                             procedure (This : in out Object;
                                        Undo :    out Undo_Info);
                           Undoer  : not null access
                             procedure (This : in out Object;
                                        Undo : in     Undo_Info);
                           Weight  : in     Float   := 1.0);
   --  Probabilities for each mutation are automatically computed from the
   --  weights given here.

   procedure Identity      (This : in out Object; Undo : out Undo_Info);
   procedure Undo_Identity (This : in out Object; Undo : in  Undo_Info);
   --  Test mutation, does nothing!

   -----------------
   -- CONVERSIONS --
   -----------------

   function To_Assignment   (This : in Object) return Cr.Assignment.Object;
   --  Obtain an assignment from the current configuration.

   procedure Set_Assignment (This : in out Object;
                             Ass  : in     Cr.Assignment.Object);
   --  The assignment given will be used as current solution.
   --  Should check that no pending tasks are unassigned.

private

   type Object is tagged null record;

   type Undo_Info is null record;

end Agpl.Cr.Mutable_Assignment;
