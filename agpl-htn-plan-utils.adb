with Agpl.Htn.Tasks.Lists_Utils;
with Agpl.Random;

package body Agpl.Htn.Plan.Utils is

   -----------------------
   -- Get_Any_Expansion --
   -----------------------

--     function Get_Any_Expansion (This : in Plan.Object) return Plan.Object is
--
--        Result : Plan.Object := Empty_Plan;
--
--        -- Receive_Plan --
--        procedure Receive_Plan (A_Plan : in Plan.Object) is
--        begin
--           if Result.Is_Empty then
--              Result := A_Plan;
--           end if;
--        end Receive_Plan;
--
--     begin
--        Expand (This, Receive_Plan'Access);
--        return Result;
--     end Get_Any_Expansion;

   -----------------------
   -- Get_Any_Expansion --
   -----------------------

   function Get_Any_Expansion (This : in Plan.Object;
                               Jobs : in Tasks.Lists.List)
                               return    Tasks.Lists.List
   is
      use Tasks.Lists;
      I      : Cursor := Jobs.First;
      Result : List;
   begin
      while Has_Element (I) loop
         if Element (I).Is_Primitive then
            Result.Append (Element (I));
         else
            declare
               Some_Plan : Plan.Object := This;
            begin
               Some_Plan.Add_Task (Element (I));
               Some_Plan := Get_Any_Expansion (Some_Plan);
               if Some_Plan.Is_Empty then
                  raise Constraint_Error;
               end if;
               declare
                  Jobs : List;
               begin
                  Plan.Enumerate_Tasks (Some_Plan,
                                        Jobs,
                                        Primitive => True,
                                        Pending   => True);
                  Tasks.Lists_Utils.Concatenate (Result, Jobs);
               end;
            end;
         end if;
         Next (I);
      end loop;

      return Result;
   end Get_Any_Expansion;

   -----------------------
   -- Get_Any_Expansion --
   -----------------------

   function Get_Any_Expansion (This : in Plan.Object) return Plan.Object is
      Result : Plan.Object := Plan.Inflate (This);

      use Plan_Node;

      procedure Replace_Child (Parent_Node,
                               New_Child,
                               Old_Child   : in Subplan) is
         --  We need to first deep copy, since the branch will be freed:
         Fresh_Node : constant Subplan := Deep_Copy (New_Child, Parent_Node);
         Aux        :          Subplan := Old_Child;
      begin
         if Parent_Node = null then
            --  So no node, but the root
            Result.Tasks := Fresh_Node;
         else
            case Get_Kind (Parent_Node) is
               when Task_Node =>
                  pragma Assert (Get_Expansion (Parent_Node) = Old_Child);
                  Set_Child (Parent_Node, Fresh_Node, Force => True);
                  Delete (Aux);
               when And_Node =>
                  --  Look for the proper child and replace
                  declare
                     Nodes : Node_Vectors.Vector := Get_Children (Parent_Node);
                  begin
                     for I in Nodes.First_Index .. Nodes.Last_Index loop
                        if Nodes.Element (I) = Old_Child then
                           Nodes.Replace_Element (I, Fresh_Node);
                           Set_Children (Parent_Node, Nodes, Force => True);
                           Delete (Aux);
                           return;
                        end if;
                     end loop;
                     raise Program_Error; -- Should'nt arrive here, never
                  end;
               when Or_Node =>
                  raise Constraint_Error; -- Impossible to be a OR node.
            end case;
         end if;
      end Replace_Child;

      procedure Explore_Node (Node : in Subplan) is
      begin
         if Node /= null then
            case Get_Kind (Node) is
               when Task_Node =>
                  Explore_Node (Get_Expansion (Node));
               when And_Node =>
                  declare
                     Nodes : constant Node_Vectors.Vector := Get_Children (Node);
                  begin
                     for I in Nodes.First_Index .. Nodes.Last_Index loop
                        Explore_Node (Nodes.Element (I));
                     end loop;
                  end;
               when Or_Node =>
                  --  Select a random child and rebind the parent
                  declare
                     Nodes  : constant Node_Vectors.Vector := Get_Children (Node);
                     Parent : constant Subplan             := Get_Parent (Node);
                     use Node_Vectors;
                  begin
                     Replace_Child (Parent,
                                    Nodes.Element
                                      (Random.Get_Integer (First_Index (Nodes),
                                                           Last_Index  (Nodes))),
                                    Node);

                     --  And go down again! (Note that Node as been freed already!)
                     Explore_Node (Parent);
                  end;
            end case;
         end if;
      end Explore_Node;

   begin
      Explore_Node (Result.Tasks);
      Plan_Node.Build_Index (Result.Tasks, Result.Index);
      pragma Assert (Plan_Node.Is_Sane (Result.Tasks));

--      Print_Tree_Summary (Result);

      return Result;
   end Get_Any_Expansion;

end Agpl.Htn.Plan.Utils;
