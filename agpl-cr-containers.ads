pragma Warnings (Off);
with Agpl.Cr.Agent.Containers;
with Agpl.Cr.Agent.Utils;
with Agpl.Htn.Tasks.Containers;
with Agpl.Htn.Tasks.Utils;

with Ada.Containers.Indefinite_Ordered_Maps;

package Agpl.Cr.Containers is

   pragma Preelaborate;

   package Agent_Containers renames Agpl.Cr.Agent.Containers;
   package Agent_Lists   renames Agent.Containers.Lists;
   package Agent_Maps    renames Agent.Containers.Maps;
   package Agent_Vectors renames Agent.Containers.Vectors;
   package Agent_Utils   renames Agent.Utils;

   package Task_Containers renames Agpl.Htn.Tasks.Containers;
   package Task_Lists   renames Agpl.Htn.Tasks.Containers.Lists;
   package Task_Maps    renames Agpl.Htn.Tasks.Containers.Maps;
   package Task_Vectors renames Agpl.Htn.Tasks.Containers.Vectors;
   package Task_Utils   renames Agpl.Htn.Tasks.Utils;

   package String_Cost_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Costs);

end Agpl.Cr.Containers;
