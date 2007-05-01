with Agpl.Htn.Tasks.Containers;
with Agpl.Htn.Tasks;
use  Agpl;

package Agpl.Htn.Tasks.Extra is

   pragma Preelaborate;

   function Contains (T  : Htn.Tasks.Containers.Lists.List;
                      Id : Htn.Tasks.Task_Id)
                      return Boolean;

end Agpl.Htn.Tasks.Extra;
