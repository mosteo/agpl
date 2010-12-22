with Agpl.Constants; use Agpl.Constants;
with Agpl.Drawing.Predrawer.Generic_Color;

package Agpl.Drawing.Predrawer.Stock is

   procedure Color_Black is new Generic_Color (Black);
   procedure Color_White is new Generic_Color (White);

   procedure Color_Red is new Generic_Color (Red);

   procedure Color_Dark_Green is new Generic_Color (Dark_Green);
   procedure Color_Navy       is new Generic_Color (Navy);
   procedure Color_Silver     is new Generic_Color (Silver);

end Agpl.Drawing.Predrawer.Stock;
