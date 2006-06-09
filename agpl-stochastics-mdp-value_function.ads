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
--  $Id: agpl.ads,v 1.4 2004/01/21 21:05:25 Jano Exp $

--  Root package for MDP solvers.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

with Ada.Containers.Indefinite_Hashed_Maps;

package Agpl.Stochastics.Mdp.Value_Function is

   Unknown_Value : exception;

   type Object is private;

   function Contains
     (This : in Object;
      S    : in State.Object'Class) return Boolean;

   function Contains
     (This : in Object;
      S    : in State.Object_Id) return Boolean;

   function Get_Action
     (This : in Object;
      S    : in State.Object'Class) return Action.Object'Class;

   function Get_Action
     (This : in Object;
      S    : in State.Object_Id) return Action.Object'Class;

   function Get_Value
     (This : in Object;
      S    : in State.Object'Class) return Rewards;

   function Get_Value
     (This : in Object;
      S    : in State.Object_Id) return Rewards;

   procedure Set_Value
     (This  : in out Object;
      S     : in     State.Object'Class;
      Value : in     Rewards;
      A     : in     Action.Object'Class);

   procedure Summary (This : in Object);
   --  Stderr dump of State id -> Rewards

   -----------------
   -- Enumeration --
   -----------------

   type Cursor is private;

   function First (This : in Object) return Cursor;

   function Is_Valid (This : in Cursor) return Boolean;

   procedure Next (This : in out Cursor);
   --  May render invalid a cursor.

   function Get_Action
     (This : in Object; I : in Cursor) return Action.Object'Class;

   function Get_Reward
     (This : in Object; I : in Cursor) return Rewards;

   function Get_State
     (This : in Object; I : in Cursor) return State.Object'Class;

   function Get_State_Id
     (This : in Object; I : in Cursor) return State.Object_Id;

private

   package Value_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (State.Object_Id, Rewards, State.Hash, State."=");

   package Action_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (State.Object_Id,
      Action.Object'Class,
      State.Hash,
      State."=",
      Action."=");

   type Object is record
      Values  : Value_Maps.Map;
      --  This holds the best reward attainable from a given state.

      Actions : Action_Maps.Map;
      --  This holds the action which yields such reward.

      States  : State.Object_Maps.Map;
      --  This holds the states.
   end record;

   type Cursor is record
      Valid : Boolean := False;
      Pos   : Value_Maps.Cursor;
   end record;

end Agpl.Stochastics.Mdp.Value_Function;
