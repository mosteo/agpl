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

--  The difference with Expres.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

with Agpl.Trace;   use Agpl.Trace;

package body Agpl.Cr.Mutable_Assignment.Moves is

   use type Htn.Tasks.Task_Id;

   ------------------
   -- Do_Move_Task --
   ------------------

   procedure Do_Move_Task (This : in out Object;
                           Undo :    out Undo_Info)
   is
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);
         U        : Undo_Internal (Move_Task);
      begin
         U.Description := + "MOVE";

         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         declare
            Target    : Task_Context_Ptr;

            New_Prev,
            New_Next  : Task_Context_Ptr;
         begin
            This.Select_Random_Insertion (All_Assigned_Tasks,
                                          New_Prev,
                                          Target,
                                          New_Next);
            declare
               New_Owner : constant Agent_Id := Get_Attribute (Target, Owner);
            begin
               Do_Insert_Task (This,
                               New_Prev,
                               Src_Copy,
                               New_Next,
                               New_Owner);
            end;
         end;
      end;
   end Do_Move_Task;

   ---------------------------------
   -- Do_Move_Task_Changing_Owner --
   ---------------------------------

   procedure Do_Move_Task_Changing_Owner (This : in out Object;
                                          Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "MOVE+OWNER";

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);

         New_Owner : constant Agent_Id :=
                       Agent_Id
                         (+ Agent_Context
                            (This.Select_Random_Context
                               (All_Agents).all).Agent_Name);

      begin
         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         declare
            Prev, Curr, Next : Task_Context_Ptr;
         begin
            This.Select_Random_Insertion
              (Agent_Tasks_Bag (New_Owner),
               Prev,
               Curr,
               Next);
            This.Do_Insert_Task (Prev,
                                 Src_Copy,
                                 Next,
                                 New_Owner);
         end;
      end;
   end Do_Move_Task_Changing_Owner;

   ----------------------------------------
   -- Do_Guided_Move_Task_Changing_Owner --
   ----------------------------------------

   procedure Do_Guided_Move_Task_Changing_Owner (This : in out Object;
                                                 Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "MOVE+GUIDED+OWNER";

      declare
         Worst_Agent : constant Agent_Id :=
                         Agent_Id (+This.Minmax.Last_Element.Agent);
         Src         : Task_Context_Ptr :=
                         This.Select_Random_Task (Agent_Tasks_Bag (Worst_Agent));
         Src_Copy : Task_Context := Task_Context (Src.all);
         New_Owner : constant Agent_Id :=
                       Agent_Id
                         (+ Agent_Context
                            (This.Select_Random_Context
                               (All_Agents).all).Agent_Name);
      begin
         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         declare
            Prev, Curr, Next : Task_Context_Ptr;
         begin
            This.Select_Random_Insertion
              (Agent_Tasks_Bag (New_Owner),
               Prev,
               Curr,
               Next);
            This.Do_Insert_Task (Prev,
                                 Src_Copy,
                                 Next,
                                 New_Owner);
         end;
      end;
   end Do_Guided_Move_Task_Changing_Owner;

   -------------------
   -- Do_Swap_Order --
   -------------------

   procedure Do_Swap_Order (This : in out Object;
                            Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "SWAP ORDER";

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);
         Next     : constant Task_Context_Ptr :=
                      This.Get_Task_Context (Src.Next);
      begin
         if Next /= null then
            This.Add_Undo_Move (Src, U);
            Undo.Handle.Set (U);
            This.Do_Remove_Task (Src);
            This.Do_Insert_Task (Next,
                                 Src_Copy,
                                 This.Get_Task_Context (Next.Next),
                                 Agent_Id (Get_Attribute (Next, Owner)));
         else
            This.Do_Identity (Undo);
         end if;
      end;
   end Do_Swap_Order;

   -------------------
   -- Do_Swap_Tasks --
   -------------------

   procedure Do_Swap_Tasks (This : in out Object;
                            Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 3 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "SWAP ANY";

      declare
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context     := Task_Context (Src.all);
         Prev_1   : constant Htn.Tasks.Task_id := Src.Prev;
         Next_1   : constant Htn.Tasks.Task_id := Src.Next;
         Owner_1  : constant Agent_Id          :=
                      Agent_Id (Get_Attribute (Src, Owner));
      begin
         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         declare
            Target      : Task_Context_Ptr;

            New_Prev,
            New_Next    : Task_Context_Ptr;
            Watchdog    : Natural := 0;
         begin
            loop
               This.Select_Random_Insertion (All_Assigned_Tasks,
                                             New_Prev,
                                             Target,
                                             New_Next);
               exit when Target.Job /= Prev_1 and then
                         Target.Job /= Next_1;
               Watchdog := Watchdog + 1;
               if Watchdog > 100 then
                  raise Program_Error
                    with "Moving target failed (" &
                         This.Num_Assigned_Tasks'Img & " assigned tasks)";
               end if;
            end loop;

            declare
               Target_Copy : Task_Context      := Task_Context (Target.all);
               New_Owner   : constant Agent_Id := Get_Attribute (Target, Owner);
            begin
               Do_Insert_Task (This,
                               New_Prev,
                               Src_Copy,
                               New_Next,
                               New_Owner);

               This.Add_Undo_Move (Target, U);
               Undo.Handle.Set (U);
               This.Do_Remove_Task (Target);

               if Prev_1 /= No_Task then
                  declare
                     Prev : constant Task_Context_Ptr :=
                              This.Get_Task_Context (Prev_1);
                     Next : constant Task_Context_Ptr :=
                              This.Get_Task_Context (Prev.Next);
                  begin
                     This.Do_Insert_Task
                       (Prev, Target_Copy, Next, Owner_1);
                  end;
               elsif Next_1 /= No_Task then
                  This.Do_Insert_Task
                    (This.Get_Task_Context
                       (This.Get_Task_Context (Next_1).Prev),
                     Target_Copy,
                     This.Get_Task_Context (Next_1),
                     Owner_1);
               else
                  This.Do_Insert_Task
                    (null, Target_Copy, null, Owner_1);
               end if;
            end;
         end;
      end;
   end Do_Swap_Tasks;

   --------------------
   -- Undo_Move_Task --
   --------------------

   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info) is
      U : Undo_Internal renames Undo.Handle.Ref.all;
   begin
      case U.Kind is
         when Identity =>
            null;
         when Move_Task =>
            for I in reverse U.Move_Stack.First .. U.Move_Stack.Last loop
               declare
                  Move : Undo_Move_Task_Info renames U.Move_Stack.Vector (I);
                  Src  : Task_Context_Ptr :=
                           This.Get_Task_Context (Move.Moved_One);
               begin
                  This.Do_Move_Task
                    (After_This  => This.Get_Task_Context (Move.Was_After),
                     Src         => Src,
                     Before_This => This.Get_Task_Context (Move.Was_Before),
                     New_Owner   => Agent_Id (+Move.Owner_Was));

                  if Move.Minsum_Was /= This.Minsum then
                     Log ("Cost was " & Cr.Image (Move.Minsum_Was, 10) &
                          " but is " & Cr.Image (This.Minsum, 10) &
                          " (" & Cr.Image (This.Minsum - Move.Minsum_Was, 10) & ")",
                          Error, Log_Section);
                     --  Cr.Cost_Matrix.Print (This.Context.Ref.Costs);
                     This.Reevaluate_Costs;
                     --  raise Program_Error with "Undo breached cost integrity!";
                  end if;
               end;
            end loop;
         when others =>
            raise Program_Error;
      end case;
   end Undo_Move_Task;

end Agpl.Cr.Mutable_Assignment.Moves;
