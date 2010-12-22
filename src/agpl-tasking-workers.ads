with Agpl.Tasking.Code;
with Agpl.Tasking.Generic_Workers;

package Agpl.Tasking.Workers is
new Agpl.Tasking.Generic_Workers (Agpl.Tasking.Code.Object'Class,
                                  Agpl.Tasking.Code."=");
