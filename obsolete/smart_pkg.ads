with Agpl.Cr.Gap;
with Agpl.Protected_Value;
with Agpl.Smart_Access;
use  Agpl;

with Gnat.Debug_Pools;

package Smart_Pkg is

   Pool : Gnat.Debug_Pools.Debug_Pool;

   type Int is access Integer;
   for Int'Storage_Pool use Pool;

   package Smart is new Smart_Access (Integer, Int);

   Blah : Smart.Object := Smart.Bind (new Integer'(3));

   package Pro is new Agpl.Protected_Value (Agpl.Cr.Gap.Object_Array);

end Smart_pkg;
