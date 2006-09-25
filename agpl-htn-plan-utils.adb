with Agpl.Htn.Plan_Node;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Htn.Plan.Utils is

   -------------------
   -- Replace_Child --
   -------------------

   procedure Replace_Child (This        : in out Plan.Object;
                            Parent_Node,
                            New_Child,
                            Old_Child   : in     Subplan)
   is
      use Plan_Node;
      --  We need to first deep copy, since the branch will be freed:
      Fresh_Node : Subplan;
      Aux        : Subplan := Old_Child;
   begin
      if Is_Ancestor (Old_Child, Of_This => New_Child) then
         Log ("Old Child is ancestor of new one",
              Debug, Section => Log_Section);
         Fresh_Node := Deep_Copy (New_Child, Parent_Node);
      else
         Log ("Old Child is NOT ancestor of new one",
              Debug, Section => Log_Section);
         Fresh_Node := New_Child;
      end if;

      if Parent_Node = null then
         --  So no node, but the root
         Log ("Replacing root", Debug, Section => Log_Section);
         This.Tasks := Fresh_Node;
      else
         case Get_Kind (Parent_Node) is
            when Task_Node =>
               Log ("Replacing expansion of task node: " &
                    Get_Task (Parent_Node).all.To_String,
                    Debug, Section => Log_Section);
               pragma Assert (Get_Expansion (Parent_Node) = Old_Child);
               Set_Child (Parent_Node, Fresh_Node, Force => True);
               Delete (Aux);
            when And_Node | Or_Node =>
               Log ("Replacing child of " & Get_Kind (Parent_Node)'Img,
                    Debug, Section => Log_Section);
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
         end case;
      end if;
   end Replace_Child;

   ----------------------
   -- Trim_Or_Siblings --
   ----------------------

   procedure Trim_Or_Siblings (This : in out Plan.Object;
                               Job  : in     Tasks.Task_Id)
   is
      use Plan_Node;
      Node           : Subplan := This.Get_Node (Job);
      Prev,
      Parent         : Subplan;
      Reindex_Needed : Boolean := False;
   begin
      if Node /= null then
         pragma Assert (Get_Kind (Node) = Task_Node);
         null;
      end if;
      while Node /= null loop
         if Get_Kind (Node) = Or_Node then
            Parent := Get_Parent (Node);
            --  Replace the OR node by the branch we were arriving from.
            Log ("Calling Replace_Child with Old_Child of OR kind",
                 Debug, Section => Log_Section);
            Reindex_Needed :=
              Reindex_Needed or else Is_Ancestor (Node, Of_This => Prev);
            Replace_Child (This,
                           Parent_Node => Parent,
                           New_Child   => Prev,
                           Old_Child   => Node);
            Log ("Trim_OR_Siblings: OR replaced",
                 Debug, Section => Log_Section);
--            This.Print_Tree_Summary;
            Node := Parent;
            Log ("Node = " & Get_Id (Node), Debug, Section => Log_Section);
         else
            Prev := Node;
            Node := Get_Parent (Node);
         end if;
         exit when Node = null;
      end loop;
      Log ("End of branch trimming", Debug, Section => Log_Section);

      if Reindex_Needed then
         This.Build_Index;
      end if;
   end Trim_Or_Siblings;

end Agpl.Htn.Plan.Utils;
