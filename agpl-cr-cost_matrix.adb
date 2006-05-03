with Agpl.Cr.Tasks.Starting_Pose;

package body Agpl.Cr.Cost_Matrix is

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
     (Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List) return Htn.Tasks.Lists.List
   is
      Tasks_Bis : Htn.Tasks.Lists.List  := Tasks;
      I         : Cr.Agent.Lists.Cursor := Agents.Last;
      use Cr.Agent.Lists;
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
      Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List)
   is
      package AL renames Cr.Agent.Lists;
      package TL renames Htn.Tasks.Lists;
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
               end if;

               TL.Next (Fin);
            end loop;
            TL.Next (Ini);
         end loop;
         AL.Next (A);
      end loop;
   end Create;

   -----------------------
   -- Create_With_Start --
   -----------------------

   function Create_With_Start
     (Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List) return Object
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
      Agents : in Cr.Agent.Lists.List;
      Tasks  : in Htn.Tasks.Lists.List)
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
         return Costs'Last;
      else
         return Element (I);
      end if;
   end Get_Cost;

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
