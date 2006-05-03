with Ada.Strings.Hash;

package body Agpl.Cr.Agent_Task_Key is

   ---------
   -- Get --
   ---------

   function Get
     (A : in Cr.Agent.Object'Class;
      T : in Htn.Tasks.Object'Class)
      return Object
   is
   begin
      return (Agent => +(A.Get_Name),
              Tid   => T.Get_Id);
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash (This : in Object) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (+This.Agent & ":x:" & Htn.Tasks.Task_Id'Image (This.Tid));
   end Hash;

end Agpl.Cr.Agent_Task_Key;
