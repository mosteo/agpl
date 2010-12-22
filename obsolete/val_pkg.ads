with Agpl.Protected_Value;
use  Agpl;

package Val_Pkg is

   type Integer_Array is array (Integer range <>) of Integer;

   package Protected_Integers is new Agpl.Protected_Value (Integer_Array);

end Val_Pkg;
