with Agpl.Strings.Fields;

package body Agpl.Cr is

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
