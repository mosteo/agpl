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

with Ada.Numerics.Elementary_Functions;

package body Agpl.Cr.Mutable_Assignment.Auctions is

   ---------------------
   -- Do_Auction_Task --
   ---------------------

   procedure Do_Auction_Task (This : in out Object;
                              Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "LOG AUCTION";

      declare
         use Ada.Numerics.Elementary_Functions;
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);
         Checks   : constant Positive :=
                      Natural (Log (Float (This.Num_Assigned_Tasks))) + 1;

         Best_Prev,
         Best_Next   : Task_Context_Ptr;
         Best_Cost   : Costs := Infinite;
         Best_Name   : Ustring;
      begin
         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         Log ("Checking" & Checks'Img & " of" & This.Num_Assigned_Tasks'Img &
              " possible insertions", Debug, Detail_Section);

         for I in 1 .. Checks loop
            declare
               Curr_Target,
               Curr_Prev,
               Curr_Next : Task_Context_Ptr;
               Curr_Cost : Costs;
            begin
               This.Select_Random_Insertion (All_Assigned_Tasks,
                                             Curr_Prev,
                                             Curr_Target,
                                             Curr_Next);
               Curr_Cost := This.Evaluate_Cost_Inserting
                 (Curr_Prev,
                  Src_Copy.Job,
                  Curr_Next,
                  Agent_Id (Get_Attribute (Curr_Target, Owner)));

               if Curr_Cost < Best_Cost then
                  Best_Cost := Curr_Cost;
                  Best_Prev := Curr_Prev;
                  Best_Next := Curr_Next;
                  Best_Name := +String (Get_Attribute (Curr_Target, Owner));
               end if;
            end;
         end loop;

         if Best_Cost < Cr.Infinite then
            This.Do_Insert_Task (Best_Prev,
                                 Src_Copy,
                                 Best_Next,
                                 Agent_Id (+Best_Name));
         else
            This.Do_Insert_Task (This.Get_Task_Context (Src_Copy.Prev),
                                 Src_Copy,
                                 This.Get_Task_Context (Src_Copy.Next),
                                 Get_Attribute (Src_Copy, Owner));
            This.Do_Identity (Undo);
         end if;
      end;
   end Do_Auction_Task;

   --------------------------------
   -- Do_Exhaustive_Auction_Task --
   --------------------------------

   procedure Do_Exhaustive_Auction_Task (This : in out Object;
                                         Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "FULL AUCTION";

      declare
         use Ada.Numerics.Elementary_Functions;
         Src      : Task_Context_Ptr :=
                      This.Select_Random_Task (All_Assigned_Tasks);
         Src_Copy : Task_Context := Task_Context (Src.all);

         Best_Prev,
         Best_Next   : Task_Context_Ptr;
         Best_Cost   : Costs := Infinite;
         Best_Name   : Ustring;

         procedure Do_It (I : in Solution_Context_Maps.Cursor) is
            use Solution_Context_Maps;
         begin
            if Element (I) in Task_Context then
               declare
                  C    : constant Task_Context := Task_Context (Element (I));
                  Prev : constant Task_Context_Ptr :=
                           This.Get_Task_Context (C.Prev);
                  Next : constant Task_Context_Ptr :=
                           This.Get_Task_Context (C.Job);
                  Cost : Costs;
               begin
                  Cost := This.Evaluate_Cost_Inserting
                    (Prev,
                     Src_Copy.Job,
                     Next,
                     Agent_Id (Get_Attribute (C, Owner)));

                  if Cost < Best_Cost then
                     Best_Cost := Cost;
                     Best_Prev := Prev;
                     Best_Next := Next;
                     Best_Name := +String (Get_Attribute (C, Owner));
                  end if;
               end;
            end if;
         end Do_It;

      begin
         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         This.Contexts.Iterate (Do_It'Access);

         if Best_Cost < Cr.Infinite then
            This.Do_Insert_Task (Best_Prev,
                                 Src_Copy,
                                 Best_Next,
                                 Agent_Id (+Best_Name));
         else
            This.Do_Insert_Task (This.Get_Task_Context (Src_Copy.Prev),
                                 Src_Copy,
                                 This.Get_Task_Context (Src_Copy.Next),
                                 Get_Attribute (Src_Copy, Owner));
            This.Do_Identity (Undo);
         end if;
      end;
   end Do_Exhaustive_Auction_Task;

   ----------------------------
   -- Do_Guided_Auction_Task --
   ----------------------------

   procedure Do_Guided_Auction_Task (This : in out Object;
                                     Undo :    out Undo_Info)
   is
      U : Undo_Internal (Move_Task);
   begin
      if This.Num_Assigned_Tasks <= 1 then
         This.Do_Identity (Undo);
         return;
      end if;

      U.Description := + "GUIDED+LOG AUCTION";

      declare
         use Ada.Numerics.Elementary_Functions;
         Worst_Agent : constant Agent_Id :=
                         Agent_Id (+This.Minmax.Last_Element.Agent);
         Best_Agent  : constant Agent_Id :=
                         Agent_Id (+This.Minmax.First_Element.Agent);
         Src         : Task_Context_Ptr :=
                         This.Select_Random_Task (Agent_Tasks_Bag (Worst_Agent));
         Src_Copy    : Task_Context := Task_Context (Src.all);
         Checks      : constant Positive :=
                         Natural (Log (Float (This.Num_Assigned_Tasks))) + 1;

         Best_Prev,
         Best_Next   : Task_Context_Ptr;
         Best_Cost   : Costs := Infinite;
         Best_Name   : Ustring;
      begin
         This.Add_Undo_Move (Src, U);
         Undo.Handle.Set (U);
         This.Do_Remove_Task (Src);

         Log ("Checking" & Checks'Img & " of" & This.Num_Assigned_Tasks'Img &
              " possible insertions", Debug, Detail_Section);

         for I in 1 .. Checks loop
            declare
               Curr_Target,
               Curr_Prev,
               Curr_Next : Task_Context_Ptr;
               Curr_Cost : Costs;
            begin
               This.Select_Random_Insertion (Agent_Tasks_Bag (Best_Agent),
                                             Curr_Prev,
                                             Curr_Target,
                                             Curr_Next);
               Curr_Cost := This.Evaluate_Cost_Inserting
                 (Curr_Prev,
                  Src_Copy.Job,
                  Curr_Next,
                  Best_Agent);
               if Curr_Cost < Best_Cost then
                  Best_Cost := Curr_Cost;
                  Best_Prev := Curr_Prev;
                  Best_Next := Curr_Next;
                  Best_Name := +String (Best_Agent);
               end if;
            end;
         end loop;
         if Best_Cost < Cr.Infinite then
            This.Do_Insert_Task (Best_Prev,
                                 Src_Copy,
                                 Best_Next,
                                 Agent_Id (+Best_Name));
         else
            This.Do_Insert_Task (This.Get_Task_Context (Src_Copy.Prev),
                                 Src_Copy,
                                 This.Get_Task_Context (Src_Copy.Next),
                                 Get_Attribute (Src_Copy, Owner));
            This.Do_Identity (Undo);
         end if;
      end;
   end Do_Guided_Auction_Task;

end Agpl.Cr.Mutable_Assignment.Auctions;
