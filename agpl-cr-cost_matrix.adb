with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Cr.Agent.Dummy;
with Agpl.Cr.Tasks.Starting_Pose;
with Agpl.Trace; use Agpl.Trace;

with Ada.Containers;

package body Agpl.Cr.Cost_Matrix is

   use type Cr.Costs;
   use type Htn.Tasks.Task_Id;

   ---------
   -- Key --
   ---------

   function Key (Agent : in String;
                 Ini   : in Htn.Tasks.Task_Id;
                 Fin   : in Htn.Tasks.Task_Id) return String
   is
   begin
      return Agent & ":" & Ini'Img & ":" & Fin'Img;
   end Key;

   ------------------------
   -- Add_Starting_Tasks --
   ------------------------

   function Add_Starting_Tasks
     (Agents : in Cr.Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List) return Htn.Tasks.Containers.Lists.List
   is
      Tasks_Bis : Htn.Tasks.Containers.Lists.List  := Tasks;
      I         : Cr.Agent.Containers.Lists.Cursor := Agents.Last;
      use Cr.Agent.Containers.Lists;
   begin
      while Has_Element (I) loop
         Tasks_Bis.Prepend (Cr.Tasks.Starting_Pose.Create (Element (I).Get_Name));
         Previous (I);
      end loop;

      return Tasks_Bis;
   end Add_Starting_Tasks;

   ------------
   -- Create --
   ------------

   procedure Create
     (This   : in out Object;
      Agents : in Cr.Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List)
   is
      package AL renames Cr.Agent.Containers.Lists;
      package TL renames Htn.Tasks.Containers.Lists;
      use type AL.Cursor; use type TL.Cursor;

      A   : AL.Cursor := AL.First (Agents);
      Ini,
      Fin : TL.Cursor;
   begin
      while A /= AL.No_Element loop
         Ini := TL.First (Tasks);
         while Ini /= TL.No_Element loop
            Fin := TL.First (Tasks);
            while Fin /= TL.No_Element loop

               if Ini /= Fin then
                  Set_Cost (This,
                            Cr.Agent.Get_Name (AL.Element (A)),
                            Htn.Tasks.Get_Id (TL.Element (Ini)),
                            Htn.Tasks.Get_Id (TL.Element (Fin)),
                            Cr.Agent.Get_Cost (AL.Element (A),
                                               TL.Element (Ini),
                                               TL.Element (Fin)));
                  Set_Cost (This,
                            Cr.Agent.Get_Name (AL.Element (A)),
                            Htn.Tasks.Get_Id (TL.Element (Fin)),
                            Htn.Tasks.Get_Id (TL.Element (Ini)),
                            Cr.Agent.Get_Cost (AL.Element (A),
                                               TL.Element (Fin),
                                               TL.Element (Ini)));

                  --  Special case for the starting task
                  if Tl.Element (Ini) in Cr.Tasks.Starting_Pose.Object then
                     Set_Cost (This,
                               Cr.Agent.Get_Name (Al.Element (A)),
                               Htn.Tasks.No_Task,
                               Htn.Tasks.Get_Id (Tl.Element (Fin)),
                               Get_Cost (This,
                                         Cr.Agent.Get_Name (Al.Element (A)),
                                         Htn.Tasks.Get_Id (Tl.Element (Ini)),
                                         Htn.Tasks.Get_Id (Tl.Element (Fin))));
                  end if;
               end if;

               TL.Next (Fin);
            end loop;
            TL.Next (Ini);
         end loop;
         AL.Next (A);
      end loop;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (This   : in out Object;
      Agent  : in Cr.Agent.Object'Class;
      Tasks  : in Htn.Tasks.Containers.Lists.List)
   is
      A : Cr.Agent.Containers.Lists.List;
   begin
      A.Append (Agent);
      Create (This, A, Tasks);
   end Create;

   -----------------------
   -- Create_With_Start --
   -----------------------

   function Create_With_Start
     (Agents : in Cr.Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List) return Object
   is
      Result : Object;
   begin
      Create_With_Start (Result, Agents, Tasks);
      return Result;
   end Create_With_Start;

   -----------------------
   -- Create_With_Start --
   -----------------------

   procedure Create_With_Start
     (This   : in out Object;
      Agent  : in Cr.Agent.Object'Class;
      Tasks  : in Htn.Tasks.Containers.Lists.List)
   is
   A : Cr.Agent.Containers.Lists.List;
   begin
      A.Append (Agent);
      Create_With_Start (This, A, Tasks);
   end Create_With_Start;

   -----------------------
   -- Create_With_Start --
   -----------------------

   procedure Create_With_Start
     (This   : in out Object;
      Agents : in Cr.Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List)
   is
   begin
      Create (This, Agents, Add_Starting_Tasks (Agents, Tasks));
   end Create_With_Start;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Htn.Tasks.Task_Id;
      Fin   : in Htn.Tasks.Task_Id)
      return Costs
   is
      I : constant Cursor := Find (This.Matrix, Key (Agent, Ini, Fin));
   begin
      if I = No_Element then
         if Fin = Htn.Tasks.No_Task then
            return 0.0;
         else
            return Infinite;
         end if;
      else
         return Element (I);
      end if;
   end Get_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object;
      Agent : in Cr.Agent.Object'Class) return Costs
   is
      T    : constant Htn.Tasks.Containers.Lists.List := Agent.Get_Tasks;
      Prev :          Htn.Tasks.Task_Id    := Htn.Tasks.No_Task;
      use Htn.Tasks.Containers.Lists;

      Total,
      Partial : Cr.Costs               := 0.0;
      I       : Htn.Tasks.Containers.Lists.Cursor := T.First;
   begin
      while Has_Element (I) loop
         Partial := Get_Cost (This,
                              Cr.Agent.Get_Name (Agent),
                              Prev, Htn.Tasks.Get_Id (Element (I)));
         if Partial = Cr.Infinite then
            Total := Infinite;
         else
            Total := Total + Partial;
         end if;
         exit when Partial = Cr.Infinite;
         Prev := Htn.Tasks.Get_Id (Element (I));
         Next (I);
      end loop;
      return Total;
   end Get_Plan_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object;
      Agent : in String;
      Tasks : in Htn.Tasks.Containers.Lists.List) return Costs
   is
      Ag : Cr.Agent.Dummy.Object;
   begin
      Ag.Set_Name (Agent);
      Ag.Set_Tasks (Tasks);
      return Get_Plan_Cost (This, Ag);
   end Get_Plan_Cost;

   -----------
   -- Merge --
   -----------

   procedure Merge (Dst : in out Object; Src : in Object) is
      procedure Do_It (I : in Cursor) is
      begin
         Dst.Matrix.Include (Key (I), Element (I));
      end Do_It;
   begin
      Src.Matrix.Iterate (Do_It'Access);
   end Merge;

   -----------
   -- Print --
   -----------

   procedure Print (This : in Object) is
      procedure Do_It (I : in Cursor) is
      begin
         Log (Key (I) & ": " & To_String (Float (Element (I))), Always);
      end Do_It;
   begin
      Log ("Cost matrix dump follows:", Always);
      This.Matrix.Iterate (Do_It'Access);
   end Print;

   --------------
   -- Set_Cost --
   --------------

   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Htn.Tasks.Task_Id;
      Fin   : in     Htn.Tasks.Task_Id;
      Cost  : in     Costs)
   is
   begin
      Include (This.Matrix, Key (Agent, Ini, Fin), Cost);
   end Set_Cost;

end Agpl.Cr.Cost_Matrix;
