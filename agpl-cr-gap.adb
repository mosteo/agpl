with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Text_Io;

package body Agpl.Cr.Gap is

   ------------
   -- Create --
   ------------

   function Create (P1, P2 : Cv.Point2D;
                    Kind   : Kinds := Occlusive) return Object
   is
      use type Cv.Point2D;
   begin
      return (P1   => P1,
              P2   => P2,
              Line => Cv.Line2D (P1 ** P2),
              Kind => Kind);
   end Create;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (This : Object) return Cv.Line2D is
   begin
      return This.Line;
   end Get_Line;

   ---------------
   -- Get_Start --
   ---------------

   function Get_Start (This : Object) return Cv.Point2D is
   begin
      return This.P1;
   end Get_Start;

   -------------
   -- Get_End --
   -------------

   function Get_End (This : Object) return Cv.Point2D is
   begin
      return This.P2;
   end Get_End;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind  (This : Object) return Kinds is
   begin
      return This.Kind;
   end Get_Kind;

   ----------
   -- Dump --
   ----------

   procedure Dump (This : Object) is
      use Agpl.Text_Io;
      use type Cv.Float_Array_3;
      Origin : constant Cv.Point2D := (0.0, 0.0, 1.0);
   begin
      Put_Line ("(" & To_String (This.P1 (1)) & "," & To_String (This.P1 (2)) &
                ")-(" & To_String (This.P2 (1)) & "," & To_String (This.P2 (2)) &
                "), distance: " & To_String (Cv.Signed_Distance (This.Line,  Origin)));
   end Dump;

end Agpl.Cr.Gap;
