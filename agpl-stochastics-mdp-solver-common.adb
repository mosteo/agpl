------------------------------------------------------------------------------
--                                   AGPL                                   --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
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

with Agpl.Stochastics.Mdp.Outcome;

with Text_Io;

package body Agpl.Stochastics.Mdp.Solver.Common is

   package AL  renames Action.Object_Lists;
   package CSA renames Containers.State_Actions_Maps;
   package CSO renames Containers.State_Outcomes_Maps;
   package OL  renames Containers.Outcome_Lists;

   use type AL.Cursor;
   use type CSA.Cursor;
   use type CSO.Cursor;

   -----------------------
   -- Appliable_Actions --
   -----------------------

   procedure Appliable_Actions
     (States  : in     State.Object_Lists.List;
      A       : in     Action_Function;
      Actions :    out Containers.State_Actions_Maps.Map)
   is
      use State.Object_Lists;
      I   : Cursor := First (States);

      Pos : CSA.Cursor;
      Ok  : Boolean;
   begin
      while I /= No_Element loop
         declare
            Act : constant Action.Object_Lists.List := A (Element (I));
         begin
            Actions.Insert (State.Get_Id (Element (I)), Act, Pos, Ok);
         end;
         Next (I);
      end loop;
   end Appliable_Actions;

   ------------------------
   -- Appliable_Outcomes --
   ------------------------

   procedure Appliable_Outcomes
     (Initial  : in     State.Object'Class;
      A        : in     Action_Function;
      AE       : in     Action_Evolution_Function;
      Outcomes :    out Containers.Outcome_Lists.List)
   is
      Act : constant Action.Object_Lists.List := A (Initial);
      I   : AL.Cursor                         := AL.First (Act);
   begin
      while I /= AL.No_Element loop
         OL.Append
           (Outcomes,
            Outcome.Create (AL.Element (I), AE (Initial, AL.Element (I))));
         AL.Next (I);
      end loop;
   end Appliable_Outcomes;

   ------------------------
   -- Appliable_Outcomes --
   ------------------------

   procedure Appliable_Outcomes
     (States   : in     State.object_Lists.List;
      A        : in     Action_Function;
      AE       : in     Action_Evolution_Function;
      Outcomes :    out Containers.State_Outcomes_Maps.Map)
   is
      use State.Object_Lists;
      I   : Cursor := First (States);

      Pos : CSO.Cursor;
      Ok  : Boolean;
   begin
      while I /= No_Element loop
         declare
            S   : constant State.Object'Class       := Element (I);
            Act : constant Action.Object_Lists.List := A (S);
            J   : AL.Cursor := AL.First (Act);
            O   : OL.List;
         begin
            while J /= AL.No_Element loop
               OL.Append
                 (O,
                  Outcome.Create (AL.Element (J), AE (S, AL.Element (J))));
               AL.Next (J);
            end loop;

            --  Add a new outcome list to the state:
            CSO.Insert
              (Outcomes,
               State.Get_Id (Element (I)),
               O,
               Pos, Ok);
         end;
         Next (I);
      end loop;
   end Appliable_Outcomes;

   ----------------------
   -- Reachable_States --
   ----------------------

   procedure Reachable_States
     (Initial : in     State.Object_Lists.List;
      E       : in     Evolution_Function;
      Final   :    out State.Object_Lists.List)
   is
      use State.Object_Maps;
      use State.Object_Lists;
      I       : State.Object_Lists.Cursor;
      Known   : State.Object_Maps.Map;
      --  Here we keep track of already visited states
      Pending : State.Object_Lists.List;
      --  Here we keep track of states already to expand

      Pos   : State.Object_Maps.Cursor;
      Ok    : Boolean;
   begin
      Clear (Final);

      --  Copy Initial to Pending:
      I := First (Initial);
      while I /= State.Object_Lists.No_Element loop
         Append (Pending, Element (I));
         Next (I);
      end loop;

      I := First (Pending);
      while I /= State.Object_Lists.No_Element loop
         declare
            Visited   : State.Object'Class        := Element (I);
            Immediate : List                      := E (Visited);
            J         : State.Object_Lists.Cursor := First (Immediate);
         begin
            --  Insert visited node:
            if not Is_In (State.Get_Id (Visited), Known) then
               Insert (Known, State.Get_Id (Visited), Visited, Pos, Ok);
               pragma Assert (Ok);
            end if;

            --  Copy unvisited nodes to the pending list:
            while J /= State.Object_Lists.No_Element loop
               if not Is_In (State.Get_Id (Element (J)), Known) then
                  Append (Pending, Element (J));
               end if;
               Next (J);
            end loop;
         end;
         Next (I);
      end loop;

      --  Copy all known states to the Reachable list:
      declare
         I : State.Object_Maps.Cursor := First (Known);
      begin
         while I /= State.Object_Maps.No_Element loop
            Append (Final, Element (I));
            Next (I);
         end loop;
      end;
   end Reachable_States;

   ---------------------------
   -- Reachable_With_Policy --
   ---------------------------

   procedure Reachable_With_Policy
     (Initial : in     State.Object_Lists.List;
      AE      : in     Action_Evolution_Function;
      V       : in     Value_Function.Object;
      Final   :    out State.Object_Lists.List)
   is
      use State.Object_Maps;
      use State.Object_Lists;
      I       : State.Object_Lists.Cursor;
      Known   : State.Object_Maps.Map;
      --  Here we keep track of already visited states
      Pending : State.Object_Lists.List;
      --  Here we keep track of states already to expand

      Pos   : State.Object_Maps.Cursor;
      Ok    : Boolean;
   begin
      Clear (Final);

      --  Copy Initial to Pending:
      I := First (Initial);
      while I /= State.Object_Lists.No_Element loop
         Append (Pending, Element (I));
         Next (I);
      end loop;

      I := First (Pending);
      while I /= State.Object_Lists.No_Element loop
         declare
            Visited   : State.Object'Class        := Element (I);
            Immediate : List                      :=
                          AE (Visited, Value_Function.Get_Action (V, Visited));
            J         : State.Object_Lists.Cursor := First (Immediate);
         begin
            --  Insert visited node:
            if not Is_In (State.Get_Id (Visited), Known) then
               Insert (Known, State.Get_Id (Visited), Visited, Pos, Ok);
               pragma Assert (Ok);
            end if;

            --  Copy unvisited nodes to the pending list:
            while J /= State.Object_Lists.No_Element loop
               if not Is_In (State.Get_Id (Element (J)), Known) then
                  Append (Pending, Element (J));
               end if;
               Next (J);
            end loop;
         end;
         Next (I);
      end loop;

      --  Copy all known states to the Reachable list:
      declare
         I : State.Object_Maps.Cursor := First (Known);
      begin
         while I /= State.Object_Maps.No_Element loop
            Append (Final, Element (I));
            Next (I);
         end loop;
      end;
   end Reachable_With_Policy;

   --------------------------
   -- Summary_State_Action --
   --------------------------

   procedure Summary_State_Action
     (Initial : in     State.Object_Lists.List;
      E       : in     Evolution_Function;
      A       : in     Action_Function)
   is
      States : State.Object_Lists.List;
      Acts   : Containers.State_Actions_Maps.Map;
   begin
      Reachable_States (Initial, E, States);
      Appliable_Actions (States, A, Acts);

      declare
         use State.Object_Lists;
         use Text_Io;
         I : Cursor := First (States);
      begin
         while I /= No_Element loop
            Put_Line (State.To_String (Element (I)));
            declare
               Id : constant State.Object_Id := State.Get_Id (Element (I));
               Ac : constant AL.List := CSA.Element (CSA.Find (Acts, Id));
               J  : AL.Cursor := AL.First (Ac);
               use type AL.Cursor;
            begin
               while J /= AL.No_Element loop
                  Put_Line ("   " & Action.To_String (AL.Element (J)));
                  AL.Next (J);
               end loop;
            end;
            Next (I);
         end loop;
      end;
   end Summary_State_Action;

end Agpl.Stochastics.Mdp.Solver.Common;
