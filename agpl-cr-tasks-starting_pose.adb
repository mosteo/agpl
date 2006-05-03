package body Agpl.Cr.Tasks.Starting_Pose is

   ------------
   -- Create --
   ------------

   function Create (For_Agent : in String) return Object is
   begin
      return (Htn.Tasks.Primitive.Object with
              Name_Len   => For_Agent'Length,
              Agent_Name => For_Agent);
   end Create;

end Agpl.Cr.Tasks.Starting_Pose;
