with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Cr.Agent.Dummy;
with Agpl.Cr.Tasks.Starting_Pose;
with Agpl.Trace; use Agpl.Trace;

with Ada.Containers;

package body Agpl.Cr.Cost_Matrix is

   use type Cr.Costs;
   use type Htn.Tasks.Task_Id;
   package AC renames Agpl.Cr.Agent.Containers;
   package TC renames Agpl.Htn.Tasks.Containers;

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

   --------------
   -- Contains --
   --------------

   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Htn.Tasks.Task_Id;
      Fin   : in Htn.Tasks.Task_Id) return Boolean
   is
   begin
      return This.Matrix.Contains (Key (Agent, Ini, Fin));
   end Contains;

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
                  declare
                     Bot   : constant Cr.Agent.Object'Class := Al.Element (A);
                     Name  : constant String            := Bot.Get_Name;
                     Tini  : constant Htn.Tasks.Object'Class := Tl.Element (Ini);
                     Tfin  : constant Htn.Tasks.Object'Class := Tl.Element (Fin);
                     Iniid : constant Htn.Tasks.Task_Id := Tini.Get_Id;
                     Finid : constant Htn.Tasks.Task_Id := Tfin.Get_Id;
                  begin
                     Set_Cost (This, Name, Iniid, Finid, Bot.Get_Cost (Tini, Tfin));
                     Set_Cost (This, Name, Finid, Iniid, Bot.Get_Cost (Tfin, Tini));

                     --  Special case for the starting task
                     if Tini in Cr.Tasks.Starting_Pose.Object then
                        Set_Cost
                          (This, Name,
                           Htn.Tasks.No_Task, Finid,
                           Get_Cost (Object'Class (This), Name, Iniid, Finid));
                     end if;
                  end;
               else
                  --  Same task... may be should be Infinite, maybe zero?
                  pragma Ummmm;
                  null;
               end if;

               TL.Next (Fin);
            end loop;

            TL.Next (Ini);
         end loop;
         AL.Next (A);
      end loop;

      --  Add the No_Task specials
      A := Agents.First;
      while Al.Has_Element (A) loop
         Ini := Tasks.First;
         while Tl.Has_Element (Ini) loop

            Set_Cost (This,
                      Cr.Agent.Get_Name (Al.Element (A)),
                      Htn.Tasks.Get_Id (Tl.Element (Ini)),
                      Htn.Tasks.No_Task,
                      0.0);

            Tl.Next (Ini);
         end loop;
         Al.Next (A);
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

   -----------------------
   -- Create_Only_Start --
   -----------------------

   procedure Create_Only_Start
     (This   : in out Object;
      Agents : in Cr.Agent.Containers.Lists.List;
      Tasks  : in Htn.Tasks.Containers.Lists.List)
   is
      A : AC.Lists.Cursor := Agents.First;
      T : TC.Lists.Cursor;
   begin
      while AC.Lists.Has_Element (A) loop
         T := Tasks.First;
         while TC.Lists.Has_Element (T) loop
            Set_Cost (This,
                      Cr.Agent.Get_Name (AC.Lists.Element (A)),
                      Htn.Tasks.No_Task,
                      Htn.Tasks.Get_Id (TC.Lists.Element (T)),
                      AC.Lists.Element (A).Get_Cost (TC.Lists.Element (T)));
            TC.Lists.Next (T);
         end loop;
         AC.Lists.Next (A);
      end loop;
   end Create_Only_Start;

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
         Partial := Get_Cost (Object'Class (This),
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

   -----------------
   -- Print_Diffs --
   -----------------

   procedure Print_Diffs (L, R : in Object) is
      I : Cursor := L.Matrix.First;
   begin
      Log ("DIFFS IN COST MATRIX", Always);
      while Has_Element (I) loop
         if not R.Matrix.Contains (Key (I)) then
            Log ("Missing key: " & Key (I), Always);
         elsif Element (I) /= Element (R.Matrix.Find (Key (I))) then
            Log (Key (I) & ":" & Element (I)'Img & " /= " &
                 Key (I) & ":" & Element (R.Matrix.Find (Key (I)))'Img,
                 Always);
         end if;

         Next (I);
      end loop;
      Log ("END DIFFS", Always);
   end Print_Diffs;

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
