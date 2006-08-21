with Agpl.Strings.Fields;

package body Agpl.Cr is

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Criterion : in Assignment_Criteria;
                      Minimax   : in Costs;
                      Totalsum  : in Costs) return Costs
   is
   begin
      if Minimax = Infinite or else Totalsum = Infinite then
         return Infinite;
      else
         return Costs (Criterion.Minimax_Weight)  * Minimax +
                Costs (Criterion.Totalsum_Weight) * Totalsum;
      end if;
   end Evaluate;

   -----------
   -- Value --
   -----------

   function Value (S : in String) return Assignment_Criteria is
      use Agpl.Strings.Fields;
   begin
      return (Float'Value (String_Head (S, ' ')),
              Float'Value (String_Tail (S, ' ')));
   end Value;

end Agpl.Cr;
