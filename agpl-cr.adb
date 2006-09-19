with Agpl.Conversions;
with Agpl.Strings.Fields;

package body Agpl.Cr is

   function Img is new Conversions.Fixed_To_Str (Costs);

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Criterion : in Assignment_Criteria;
                      Minmax    : in Costs;
                      Minsum    : in Costs) return Costs
   is
   begin
      if Minmax = Infinite or else Minsum = Infinite then
         return Infinite;
      else
         return Costs (Criterion.Minmax_Weight) * Minmax +
                Costs (Criterion.Minsum_Weight) * Minsum;
      end if;
   end Evaluate;

   -----------
   -- Image --
   -----------

   function Image (C : in Costs) return String is
   begin
      return Costs'Image (C);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (C : in Costs; Decimals : in Natural) return String is
   begin
      return Img (C, Decimals);
   end Image;

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
