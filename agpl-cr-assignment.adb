------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (public@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with Agpl.Trace; use Agpl.Trace;

--  with Agpl.Cr.Agent.Handle;
with Agpl.Htn.Plan.Utils;
with Agpl.Htn.Tasks.Utils;

package body Agpl.Cr.Assignment is

   ---------
   -- Add --
   ---------

   procedure Add
     (This     : in out Object;
      Agent    : in     Cr.Agent.Object'Class;
      The_Task : in     Htn.Tasks.Object'Class)
   is
      use Cr.Agent.Maps;
   begin
      if Contains (This.Agents, Cr.Agent.Get_Name (Agent)) then
         declare
            A : Cr.Agent.Object'Class :=
                  Element (Find (This.Agents, Cr.Agent.Get_Name (Agent)));
         begin
            A.Add_Task (The_Task);
            This.Agents.Replace (A.Get_Name, A);
         end;
      else
         declare
            A : Cr.Agent.Object'Class := Agent;
         begin
            A.Clear_Tasks;
            A.Add_Task (The_Task);
            This.Agents.Insert (A.Get_Name, A);
         end;
      end if;
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      This.Agents.Clear;
      This.Ok := True;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object; Name : in String) return Boolean is
   begin
      return This.Agents.Contains (Name);
   end Contains;

   ------------------
   -- Empty_Object --
   ------------------

   function Empty_Object return Object is
   begin
      return (Cr.Agent.Maps.Empty_Map, True);
   end Empty_Object;

   -----------------
   -- Fill_Owners --
   -----------------

   procedure Fill_Owners (This : in Object; Plan : in out Htn.Plan.Object) is

      --------------------
      -- For_Each_Agent --
      --------------------

      procedure For_Each_Agent (I : Agent.Maps.Cursor) is

         -------------------
         -- For_Each_Task --
         -------------------

         procedure For_Each_Task (J : Htn.Tasks.Containers.Lists.Cursor) is
         begin
            Htn.Plan.Set_Task_Owner
              (Plan,
               Htn.Tasks.Get_Id (Htn.Tasks.Containers.Lists.Element (J)),
               Agent.Get_Name (Agent.Maps.Element (I)));
         end For_Each_Task;

         T : constant Htn.Tasks.Containers.Lists.List := Agent.Get_Tasks (Agent.Maps.Element (I));
      begin
         T.Iterate (For_Each_Task'Access);
      end For_Each_Agent;
   begin
      Agent.Maps.Iterate (This.Agents, For_Each_Agent'Access);
   end Fill_Owners;

   ---------------
   -- Get_Agent --
   ---------------

   function Get_Agent (This : in Object; Name : in String)
                       return Agent.Object'Class
   is
      use Agent.Maps;
   begin
      return Element (Find (This.Agents, Name));
   end Get_Agent;

   ---------------
   -- Set_Agent --
   ---------------

   procedure Set_Agent (This : in out Object; Agent : in Cr.Agent.Object'Class) is
   begin
      This.Agents.Include (Agent.Get_Name, Agent);
   end Set_Agent;

   -----------------
   -- Freeze_Plan --
   -----------------

   function Freeze_Plan (This : in Object;
                         P    : in Htn.Plan.Object)
                         return    Htn.Plan.Object
   is
      Result : Htn.Plan.Object := P;

      procedure Check (I : in Agent.Maps.Cursor) is
         TL : constant Htn.Tasks.Containers.Lists.List := Agent.Maps.Element (I).Get_Tasks;

         procedure Check_Tasks (T : in Htn.Tasks.Containers.Lists.Cursor) is
         begin
            Htn.Plan.Utils.Trim_Or_Siblings
              (Result, Htn.Tasks.Containers.Lists.Element (T).Get_Id);
         end Check_Tasks;
      begin
         Tl.Iterate (Check_Tasks'Access);
      end Check;
   begin
--        Log ("About to freeze plan: ", Always);
--        Result.Print_Tree_Summary;
--        Log ("With assignment: ", Always);
--        This.Print_Assignment;
      This.Agents.Iterate (Check'Access);
      return Result;
   end Freeze_Plan;

   ----------------
   -- Get_Agents --
   ----------------

   function Get_Agents (This : in Object)
                        return Agent.Containers.Lists.List
   is
      use Agent.Containers.Lists;
      use Agent.Maps;

      Result : Agent.Containers.Lists.List;

      procedure Add (X : in Agent.Maps.Cursor) is
      begin
         Append (Result, Element (X));
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Result;
   end Get_Agents;

   ------------------------------
   -- Get_Agents_Without_Tasks --
   ------------------------------

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Containers.Lists.List
   is
      use Agent.Containers.Lists;
      use Agent.Maps;

      Result : Agent.Containers.Lists.List;

      procedure Add (X : in Agent.Maps.Cursor) is
         A : Cr.Agent.Object'Class := Element (X);
      begin
         A.Clear_Tasks;
         Append (Result, A);
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Result;
   end Get_Agents_Without_Tasks;

   ------------------------------
   -- Get_Agents_Without_Tasks --
   ------------------------------

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Containers.Vectors.Vector
   is
      use Agent.Containers.Vectors;
      use Agent.Maps;

      Result : Agent.Containers.Vectors.Vector;

      procedure Add (X : in Agent.Maps.Cursor) is
         A : Cr.Agent.Object'Class := Element (X);
      begin
         A.Clear_Tasks;
         Append (Result, A);
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Result;
   end Get_Agents_Without_Tasks;

   ---------------------------
   -- Get_Most_Costly_Agent --
   ---------------------------

   function Get_Most_Costly_Agent (This : in Object) return Agent.Object'Class
   is
      use Agent.Maps;

      Best   : Cursor;
      I      : Cursor   := This.Agents.First;
      Cost   : Cr.Costs := 0.0;
   begin
      while Has_Element (I) loop
         declare
            This_Cost : constant Cr.Costs := Element (I).Get_Plan_Cost;
         begin
            if This_Cost > Cost then
               Cost := This_Cost;
               Best := I;
            end if;
         end;
         Next (I);
      end loop;
      return Element (Best);
   end Get_Most_Costly_Agent;

   -------------------
   -- Get_All_Tasks --
   -------------------

   function Get_All_Tasks (This : in Object) return Htn.Tasks.Containers.Lists.List is
      Result : Htn.Tasks.Containers.Lists.List;
      use Cr.Agent.Maps;
      I      : Cursor := This.Agents.First;
   begin
      while Has_Element (I) loop
         Htn.Tasks.Utils.Concatenate (Result, Cr.Agent.Get_Tasks (Element (I)));
         Next (I);
      end loop;

      return Result;
   end Get_All_Tasks;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks
     (This : in Object;
      Agent : in Cr.Agent.Object'Class)
      return Htn.Tasks.Containers.Lists.List
   is
      use Cr.Agent.Maps;
      Empty : Htn.Tasks.Containers.Lists.List;
   begin
      if Contains (This.Agents, Cr.Agent.Get_Name (Agent)) then
         return Cr.Agent.Get_Tasks
           (Element (Find (This.Agents, Cr.Agent.Get_Name (Agent))));
      else
         return Empty;
      end if;
   end Get_Tasks;

   ----------------------
   -- Get_Max_Min_Cost --
   ----------------------

   function Get_Max_Min_Cost (This : in Object) return Costs is
      Worst : Costs := 0.0;
      use Agent.Maps;
      I : Cursor := First (This.Agents);
   begin
      while I /= No_Element loop
         Worst := Costs'Max
           (Worst,
            Agent.Get_Plan_Cost (Element (I)));
         Next (I);
      end loop;

      return Worst;
   end Get_Max_Min_Cost;

   function Get_Max_Min_Cost (This : in Object;
                              C    : in Cost_Matrix.Object) return Costs
   is
      Worst : Costs := 0.0;
      use Agent.Maps;
      I : Cursor := First (This.Agents);
   begin
      while I /= No_Element loop
         Worst := Costs'Max
           (Worst,
            Cost_Matrix.Get_Plan_Cost (C, Element (I)));
         Next (I);
      end loop;

      return Worst;
   end Get_Max_Min_Cost;

   --------------------------
   -- Get_Cummulative_Cost --
   --------------------------

   function Get_Cummulative_Cost (This : in Object) return Costs is
      Cost : Costs := 0.0;
      use Agent.Maps;
      I : Cursor := First (This.Agents);
   begin
      while I /= No_Element loop
         Cost := Cost + Agent.Get_Plan_Cost (Element (I));
         Next (I);
      end loop;

      return Cost;
   end Get_Cummulative_Cost;

   function Get_Cummulative_Cost (This : in Object;
                                  C    : in Cost_Matrix.Object) return Costs
   is
      Cost : Costs := 0.0;
      use Agent.Maps;
      I : Cursor := First (This.Agents);
   begin
      while I /= No_Element loop
         Cost := Cost + Cost_Matrix.Get_Plan_Cost (C, Element (I));
         Next (I);
      end loop;

      return Cost;
   end Get_Cummulative_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs
   is
   begin
      return Evaluate (Criterion,
                       Minmax => Get_Max_Min_Cost (This),
                       Minsum => Get_Cummulative_Cost (This));
   end Get_Cost;

   function Get_Cost (This      : in Object;
                      C         : in Cost_Matrix.Object;
                      Criterion : in Assignment_Criteria) return Costs
   is
   begin
      return Evaluate (Criterion,
                       Minmax => Get_Max_Min_Cost (This, C),
                       Minsum => Get_Cummulative_Cost (This, C));
   end Get_Cost;

   ------------------------
   -- Invalid_Assignment --
   ------------------------

   function Invalid_Assignment return Object is
      This : Object;
   begin
      This.Ok := False;
      return This;
   end Invalid_Assignment;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Object) return Boolean is
   begin
      return This.Ok;
   end Is_Valid;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid (This : in out Object; Valid : in Boolean := True) is
   begin
      This.Ok := Valid;
   end Set_Valid;

   ----------------------
   -- Print_Assignment --
   ----------------------

   procedure Print_Assignment (This : in Object) is
   begin
      Log ("** Assignment summary **", Always);

      if not Is_Valid (This) then
         Log ("Invalid assignment!", Always);
      end if;

      declare
         use Agpl.Cr.Agent.Maps;
         I : Cursor := First (This.Agents);
      begin
         while Has_Element (I) loop

            Log ("** Agent : " & Key (I), Always);

            declare
               T : constant Htn.Tasks.Containers.Lists.List := Element (I).Get_Tasks;
               J : Htn.Tasks.Containers.Lists.Cursor        := T.First;
               use Htn.Tasks.Containers.Lists;
            begin
               if not Has_Element (J) then
                  Log ("No tasks", Always);
               end if;

               while Has_Element (J) loop
                  Log (Element (J).Get_Id'Img & "-" & Element (J).To_String,
                       Always);
                  Next (J);
               end loop;
            end;
            Next (I);
            Log ("", Always);
         end loop;
      end;
   end Print_Assignment;

end Agpl.Cr.Assignment;
