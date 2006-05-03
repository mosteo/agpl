package body Agpl.Htn.Tasks.Still is

   ------------
   -- Create --
   ------------

   function Create
     (Period : in Duration;
      Start  : in Time := Clock)
      return Object
   is
   begin
      return (Tasks.Primitive.Object with
              Start  => Start,
              Period => Period);
   end Create;

   --------------
   -- Is_Still --
   --------------

   function Is_Still (This    : in Object;
                      Instant : in Time := Clock) return Boolean
   is
   begin
      return Instant >= This.Start and then Instant < This.Start + This.Period;
   end Is_Still;

end Agpl.Htn.Tasks.Still;
