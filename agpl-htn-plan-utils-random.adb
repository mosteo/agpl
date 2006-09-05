with Agpl.Htn.Plan_Node;
with Agpl.Htn.Tasks.Utils;
with Agpl.Random;

package body Agpl.Htn.Plan.Utils.Random is

   -----------------------
   -- Get_Any_Expansion --
   -----------------------

   function Get_Any_Expansion (This : in Plan.Object;
                               Jobs : in Tasks.Containers.Lists.List)
                               return    Tasks.Containers.Lists.List
   is
      use Tasks.Containers.Lists;
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
                  Tasks.Utils.Concatenate (Result, Jobs);
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
                     Replace_Child
                       (Result,
                        Parent,
                        Nodes.Element
                          (Agpl.Random.Get_Integer
                             (First_Index (Nodes),
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

end Agpl.Htn.Plan.Utils.Random;
