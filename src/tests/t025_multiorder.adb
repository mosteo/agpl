with Agpl.Containers.Indefinite_Multiordered_Maps;

with Agpl.Text_Io; use Agpl.Text_Io;
with Agpl.Trace;   use Agpl.Trace;

procedure T025_Multiorder is
   type Orders is (Direct, Inverse);

   type Key_Type (Order : Orders := Direct) is record
      case Order is
         when Direct =>
            N : Natural;
         when Inverse =>
            C : Character;
      end case;
   end record;

   function "<" (L, R : Key_Type) return Boolean is
   begin
      case L.Order is
         when Direct =>
            return L.N < R.N;
         when Inverse =>
            return L.C < R.C;
      end case;
   end "<";

   package Mmaps is new Agpl.Containers.Indefinite_Multiordered_Maps
     (Key_Type,
      Orders,
      Integer);

   M : Mmaps.Map;
begin
   Put_Line ("Starting...");

   M.Include (((Direct, 1), (Inverse, 'z')), 1);
   M.Include (((Direct, 2), (Inverse, 'y')), 2);
   M.Include (((Direct, 3), (Inverse, 'x')), 3);

   Put_Line ("First direct :" & M.First_Element (Direct).all'Img);
   Put_Line ("First inverse:" & M.First_Element (Inverse).all'Img);

   M.Exclude (Inverse, (Inverse, 'z'));

   Put_Line ("First direct :" & M.First_Element (Direct).all'Img);
   Put_Line ("First inverse:" & M.First_Element (Inverse).all'Img);

   Put_Line ("Done.");
exception
   when E : others =>
      Put_Line (Report (E));
end T025_Multiorder;
