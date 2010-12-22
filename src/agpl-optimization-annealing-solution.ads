with Agpl.Containers.Naked_Vectors;
with Agpl.Ustrings;

package Agpl.Optimization.Annealing.Solution is

--  Helper base class that has support for varied probability mutations
--  Extend it with missing bits

   type Object is abstract tagged private;

   type Mutation_Doer   is access procedure (This : in out Object'Class);
   type Mutation_Undoer is access procedure (This : in out Object'Class);

   not overriding
   procedure Add_Mutation (This    : in out Object;
                           Name    :        String;
                           Mutator : not null Mutation_Doer;
                           Undoer  : not null Mutation_Undoer;
                           Weight  :        Float   := 1.0);
   --  Probabilities for each mutation are automatically computed from the
   --  weights given here.

   not overriding
   function Last_Mutation (This : Object) return String;

   not overriding
   procedure Mutate (This : in out Object);
   --  Randomly select a mutator and apply it

   not overriding
   procedure Undo (This : in out Object);
   --  Apply the appropriate undoer

private

   use Agpl.Ustrings;

   --  Each registered mutation to be used
   type Mutation_Handler is record
      Name   : Ustring;
      Doer   : Mutation_Doer;
      Undoer : Mutation_Undoer;
      Weight : Float;
      Prob   : Optimization.Annealing.Probability;
   end record;

   package Mutation_Vectors is new Containers.Naked_Vectors (Mutation_Handler);

   type Object is tagged record
      Mutations          : Mutation_Vectors.Object (First => 1);

      Last_Mutation_Name : Ustring;
      Last_Mutation_Undo : Mutation_Undoer;
   end record;

end Agpl.Optimization.Annealing.Solution;
