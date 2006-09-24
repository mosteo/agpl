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

with Agpl.Htn.Plan_Node;
with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;

with Ada.Containers;

package body Agpl.Cr.Mutable_Assignment.Or_Mutations is

   Expensive_Checks : constant Boolean := False;

   use type Ada.Containers.Count_Type;

   -----------------------
   -- Do_Switch_Or_Node --
   -----------------------

   procedure Do_Switch_Or_Node (This : in out Object;
                                Undo :    out Undo_Info)
   is
      U : Undo_Internal (Switch_Or_Node);

      Placeholder_Usable : Boolean := False;
      Placeholder        : Undo_Move_Task_Info;

      procedure Descend_Adding (Node : in     Htn.Plan.Subplan) is
         use Htn.Plan_Node;
      begin
         case Get_Kind (Node) is
            when Task_Node =>
               if Get_Expanded (Node) then
                  --  Compound, do nothing:
                  Descend_Adding (Get_Expansion (Node));
               else
                  --  Create a new task and insert at random
                  declare
                     Tc     : Task_Context;
                  begin
                     Tc.Job := Get_Task (Node).all.Get_Id;
                     if Placeholder_Usable then
                        Placeholder_Usable := False;
                        This.Do_Insert_Task
                             (This.Get_Task_Context (Placeholder.Was_After),
                              Tc,
                              This.Get_Task_Context (Placeholder.Was_Before),
                              + Placeholder.Owner_Was);
                     else
                        declare
                           Target : constant Task_Context_Ptr :=
                                      This.Select_Random_Task (All_Assigned_Tasks);
                        begin
                           This.Do_Insert_Task
                             (This.Get_Task_Context (Target.Prev),
                              Tc,
                              Target,
                              Get_Attribute (Target, Owner));
                        end;
                     end if;
                  end;
               end if;
            when And_Node =>
               declare
                  Children : constant Node_Vectors.Vector :=
                               Get_Children (Node);
               begin
                  for I in Children.First_Index .. Children.Last_Index loop
                     Descend_Adding (Children.Element (I));
                  end loop;
               end;
            when Or_Node =>
               declare
                  Oc : Or_Context :=
                         (Solution_Context with
                          Node   => Node,
                          Branch => Node_Vectors.Element
                            (Get_Children (Node),
                             (Random.Get_Integer
                                (Node_Vectors.First_Index (Get_Children (Node)),
                                 Node_Vectors.Last_Index (Get_Children (Node))))));
               begin
                  This.Add_To_Bag (Oc, All_Active_Or_Nodes);
                  This.Contexts.Insert (Oc.Key, Oc);
               end;
         end case;
      end Descend_Adding;

      use Htn.Plan_Node;
   begin
      if This.Bag_Length (All_Active_Or_Nodes) < 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      declare
         Target : constant Solution_Context_Ptr :=
                    This.Select_Random_Context (All_Active_Or_Nodes);
         Ctx    : Or_Context renames Or_Context (Target.all);

         Children : Node_Vectors.Vector := Get_Children (Ctx.Node);
      begin

         if Children.Length <= 1 then
            This.Do_Identity (Undo);
            return;
         end if;

         U.Description := + "SWITCH OR-NODE";

         Log ("GOING TO SWITCH", Never);

         loop
            declare
               New_Child : constant Htn.Plan.Subplan :=
                             Children.Element
                               (Random.Get_Integer
                                  (Children.First_Index, Children.Last_Index));
               pragma Unbounded_Time;
            begin
               if New_Child /= Ctx.Branch then
                  U.Actived_Or_Branch := New_Child;
                  This.Descend_Removing (Ctx.Branch, U);

                  --  in 50% ocassions, we reuse the place were the switched task was:
                  if Random.Get_Integer (0, 1) = 1 then
                     Placeholder_Usable := True;
                     Placeholder := U.Or_Stack.Vector (U.Or_Stack.First);
                  end if;

                  Descend_Adding   (New_Child);
                  Ctx.Branch := New_Child;
                  exit;
               end if;
            end;
         end loop;

         Undo.Handle.Set (U);

         if Expensive_Checks and then not This.Is_Sane then
            raise Program_Error;
         end if;
      end;
   exception
      when others =>
         This.Context.Ref.Plan.Print_Tree_Summary;
         This.Debug_Dump_Contexts;
         raise;
   end Do_Switch_Or_Node;

   -----------------
   -- Undo_Switch --
   -----------------

   procedure Undo_Switch (This : in out Object; Undo : in Undo_Info) is
--        procedure Descend_Adding (Node : Htn.Subplan) is
--        begin
--           --  Re-add necessary or-nodes
--           --  Re-insert necessary tasks.
--        end Descend_Adding;
      Dummy_Undo : Undo_Internal (Switch_Or_Node);
      U          : Undo_Internal renames Undo.Handle.Ref.all;
   begin
      Log ("UNDOING SWITCH", Debug, Detail_Section);
      case U.Kind is
         when Identity =>
            null;
         when Switch_Or_Node =>
            This.Descend_Removing (U.Actived_Or_Branch, Dummy_Undo);

            --  Add all tasks:
            for I in reverse U.Or_Stack.First .. U.Or_Stack.Last loop
               declare
                  Tc : Task_Context;
               begin
                  Tc.Job := U.Or_Stack.Vector (I).Moved_One;
                  This.Do_Insert_Task
                    (After_This  =>
                       This.Get_Task_Context
                         (U.Or_Stack.Vector (I).Was_After),
                     Src         => Tc,
                     Before_This =>
                       This.Get_Task_Context
                         (U.Or_Stack.Vector (I).Was_Before),
                     New_Owner   => Agent_Id (+U.Or_Stack.Vector (I).Owner_Was));

                  This.Add_Or_Contexts (This.Context.Ref.Plan.Get_Node (Tc.Job));

                  if This.Minsum /= U.Or_Stack.Vector (I).Minsum_Was then
                     raise Program_Error
                       with "Undo (Switch) breached integrity; MinSum is" &
                     This.Minsum'Img & " but should be" &
                     U.Or_Stack.Vector (I).Minsum_Was'Img;
                  end if;

                  if Expensive_Checks and then not This.Is_Sane then
                     raise Program_Error;
                  end if;
               end;
            end loop;
         when others =>
            raise Program_Error;
      end case;

      Log ("UNDONE SWITCH", Debug, Detail_Section);
   end Undo_Switch;

end Agpl.Cr.Mutable_Assignment.Or_Mutations;
