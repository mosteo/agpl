 

with Charles.Lists.Double.Unbounded;

with System;

private package Agpl.Http.Server.Sort_handler.Aux is

   -- This ugly workaround is needed because I can't instantiate a Charles
   -- package using a generic type within a generic body.
   package Address_lists is new Charles.Lists.Double.Unbounded (
      System.Address, System."=");

end Agpl.Http.Server.Sort_handler.Aux;
