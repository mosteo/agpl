--  Exceptions of general use across Agpl.
--  And type for storage of exceptions.

with Ada.Exceptions;

package Agpl.Exceptions is

   pragma Preelaborate;

   Unimplemented : exception;

   protected type Protected_Occurrence is

      --  Maybe a more general type with a queue of exceptions could be useful?

      procedure Check_And_Clear;
      --  Will raise any stored exception or null otherwise

      function Is_Set return Boolean;

      procedure Clear;
      --  Clear any stored exception without raising it

      procedure Store (E : Ada.Exceptions.Exception_Occurrence);

   private

      Occurrence : Ada.Exceptions.Exception_Occurrence;

   end Protected_Occurrence;

end Agpl.Exceptions;
