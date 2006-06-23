with Agpl.Conversions.Io; use Agpl.Conversions.Io;
with Agpl.Cr.Tasks.Starting_Pose;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Cr.Agent is

   use type Cr.Costs;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (This     : in out Object;
      The_Task : in Htn.Tasks.Object'Class)
   is
   begin
      This.Tasks.Append (The_Task);
   end Add_Task;

   -----------------------
   -- Add_Task_Executer --
   -----------------------

   procedure Add_Task_Executer (This : in out Object;
                                Job  : in     Ada.Tags.Tag;
                                Exe  : in     Task_Executer)
   is
   begin
      Executer_Maps.Include (This.Execs, External_Tag (Job), Exe);
   end Add_Task_Executer;

   -------------------
   -- Call_Executer --
   -------------------

   procedure Call_Executer (This : in out Object;
                            Job  : in out Htn.Tasks.Object'Class;
                            Done : in out Boolean)
   is
      use Executer_Maps;
      I : constant Cursor := This.Execs.Find (External_Tag (Job'Tag));
   begin
      if Has_Element (I) then
         Element (I).all (Object'Class (This), Job, Done);
      else
         Log ("Cr.Agent.Call_Executer: Unknown task: " & External_Tag (Job'Tag),
              Warning);
      end if;
   end Call_Executer;

   -----------------
   -- Clear_Tasks --
   -----------------

   procedure Clear_Tasks (This : in out Object) is
   begin
      This.Tasks.Clear;
   end Clear_Tasks;

   -----------------------
   -- Execute_When_Idle --
   -----------------------

   procedure Execute_When_Idle
     (This : in out Object;
      Plan : in out Htn.Plan.Object)
   is
      pragma Unreferenced (This, Plan);
   begin
      null;
   end Execute_When_Idle;

   ---------------------
   -- Get_Cached_Cost --
   ---------------------

   function Get_Cached_Cost (This : in Object) return Costs is
   begin
      return This.Cost;
   end Get_Cached_Cost;

   -----------------
   -- Get_Elapsed --
   -----------------

   function Get_Elapsed (This : in Object) return Duration is
      use Ada.Calendar;
   begin
      return Clock - This.Start;
   end Get_Elapsed;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This     : in Object;
      The_Task : in Htn.Tasks.Object'Class)
      return Cr.Costs
   is
   begin
      if The_Task in Agpl.Cr.Tasks.Starting_Pose.Object then
         return 0.0;
      else
         return Get_Cost (Cr.Agent.Object'Class (This), -- Force dispatching
                          Agpl.Cr.Tasks.Starting_Pose.Create
                            (Cr.Agent.Object'Class (This).Get_Name),
                          The_Task);
      end if;
   end Get_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : in Object;
      From, To : in Htn.Tasks.Object'Class)
      return Costs
   is
      pragma Unreferenced (This, From, To);
   begin
      return Costs'Last;
   end Get_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost (This : in Object) return Costs is
   begin
      return Get_Plan_Cost (This, This.Tasks);
   end Get_Plan_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost (This  : in Object;
                           Tasks : in Htn.Tasks.Lists.List) return Costs
   is
      use Htn.Tasks.Lists;
      I     : Cursor := First (Tasks);
      Prev  : Cursor;
      Total : Costs  := 0.0;
      Cost  : Costs;
   begin
      while I /= No_Element loop
         if not Has_Element (Prev) then
            Cost := Get_Cost (Object'Class (This), Element (I));
            if Cost < Costs'Last then
               Total := Total + Cost;
            else
               return Costs'Last;
            end if;
         else
            Cost := Get_Cost (Object'Class (This), Element (Prev), Element (I));
            if Cost < Costs'Last then
               Total := Total + Cost;
            else
               return Costs'Last;
            end if;
         end if;
         Prev := I;
         Next (I);
      end loop;

      return Total;
   exception
      when Constraint_Error =>
         return Costs'Last;
   end Get_Plan_Cost;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Object) return String is
   begin
      return S (This.Name);
   end Get_Name;

   --------------------
   -- Get_First_Task --
   --------------------

   function Get_First_Task (This : in Object) return Htn.Tasks.Object'Class
   is
      use Htn.Tasks.Lists;
   begin
      return Element (First (This.Tasks));
   end Get_First_Task;

   -------------------
   -- Get_Last_Task --
   -------------------

   function Get_Last_Task  (This : in Object) return Htn.Tasks.Object'Class
   is
      use Htn.Tasks.Lists;
   begin
      return Element (Last (This.Tasks));
   end Get_Last_Task;

   ------------------
   -- Has_Executer --
   ------------------

   function Has_Executer (This : in Object;
                          Job  : in Htn.Tasks.Object'Class) return Boolean
   is
   begin
      return This.Execs.Contains (External_Tag (Job'Tag));
   end Has_Executer;

   ---------------
   -- Has_Tasks --
   ---------------

   function Has_Tasks (This : in Object) return Boolean is
      use Htn.Tasks.Lists;
   begin
      return not Is_Empty (This.Tasks);
   end Has_Tasks;

   --------------------
   -- Get_Task_Count --
   --------------------

   function Get_Task_Count (This : in Object) return Natural is
   begin
      return Natural (This.Tasks.Length);
   end Get_Task_Count;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This : in Object)
                       return Htn.Tasks.Lists.List is
   begin
      return This.Tasks;
   end Get_Tasks;

   ----------------
   -- Mark_Start --
   ----------------

   procedure Mark_Start (This : in out Object) is
   begin
      This.Start := Ada.Calendar.Clock;
      This.Cost  := Get_Plan_Cost (This);
   end Mark_Start;

   ----------------------
   -- Modify_Task_List --
   ----------------------

   procedure Modify_Task_List (This     : in out Object;
                               Modifier : access procedure
                                 (Tasks : in out Htn.Tasks.Lists.List))
   is
   begin
      Modifier (This.Tasks);
   end Modify_Task_List;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This  : in out Object;
                        Tasks : in     Htn.Tasks.Lists.List) is
   begin
      This.Tasks := Tasks;
   end Set_Tasks;

   -----------------
   -- Remove_Task --
   -----------------

   procedure Remove_Task (This : in out Object; Id : in Htn.Tasks.Task_Id) is
      use Htn.Tasks.Lists;
      use type Htn.Tasks.Task_Id;
      I : Cursor := First (This.Tasks);
   begin
      while I /= No_Element loop
         if Htn.Tasks.Get_Id (Element (I)) = Id then
            Delete (This.Tasks, I);
            return;
         end if;
         Next (I);
      end loop;
   end Remove_Task;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This : in out Object; Name : in String) is
   begin
      This.Name := U (Name);
   end Set_Name;

   ---------------------
   -- Print_Plan_Cost --
   ---------------------

   procedure Print_Plan_Cost (This : in Object)
   is
      use Htn.Tasks.Lists;
      Tasks : constant Htn.Tasks.Lists.List := This.Get_Tasks;
      Total : Cr.Costs := 0.0;

      Res   : Ustring;

      use ASU;
   begin
      Append (Res, This.Name);
      Append (Res, ": ");

      if Tasks.Is_Empty then
         Append (Res, "(0)");
      else
         Append (Res, "(");
         Append (Res, To_String (Float (Get_Cost (Object'Class (This),
                                                  First_Element (Tasks)))));
         Append (Res, ")");
         Append (Res, Tasks.First_Element.Get_Id'Img);
         Total := Total + Get_Cost (Object'Class (This), First_Element (Tasks));

         declare
            Prev : Cursor := Tasks.First;
            Curr : Cursor := Next (Tasks.First);
         begin
            while Has_Element (Curr) loop
               Append (Res, " (");
               Append (Res, To_String (Float (Get_Cost (Object'Class (This),
                                                        Element (Prev),
                                                        Element (Curr)))));
               Append (Res, ")");
               Append (Res, Element (Curr).Get_Id'Img);
               Total := Total + Get_Cost (Object'Class (This),
                                          Element (Prev),
                                          Element (Curr));

               Prev := Curr;
               Next (Curr);
            end loop;
            Append (Res, " (Total:" & To_String (Float (Total)) & ")");
         end;
      end if;

      Log (+Res, Debug);
   end Print_Plan_Cost;

end Agpl.Cr.Agent;
