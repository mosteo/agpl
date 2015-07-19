

package Agpl.Strings.Fields is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Select_field                                                       --
   ------------------------------------------------------------------------
   --  Returns the Nth field in a string, using the specified separator If no
   --  separator found, returns whole string
   function Select_Field
     (S    : in String;
      N    : in Positive;
      C    : in Character := ' ')
      return String;

   ----------------
   -- Num_Tokens --
   ----------------
   --  Num of tokens in S separated by C
   function Num_Tokens (S : String; C : Character := ' ') return Natural;
   --  0 for empty string, otherwise at least 1

   ------------------------------------------------------------------------
   -- String_Head                                                        --
   ------------------------------------------------------------------------
   --  All the string if @Separator@ is not found.
   --  Start - 1 indicates how many separators have to be skipped
   function String_Head
     (S         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String;

   function Head
     (S         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String renames String_Head;

   function H
     (S         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String renames String_Head;

   ------------------------------------------------------------------------
   -- String_Tail                                                        --
   ------------------------------------------------------------------------
   --  Returns "" if no @Separator@ found.
   --  Starts counts the amount of separators to skip over
   function String_Tail
     (S         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String;

   function Tail
     (S         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String renames String_Tail;

   function T
     (S         : String;
      Separator : Character := '/';
      Start     : Positive  := 1)
      return      String renames String_Tail;

   ------------------------------------------------------------------------
   -- String_Head_Reverse                                                --
   ------------------------------------------------------------------------
   --  These are like above, but from right to left: I.e: Tail(abc/de/fg) =
   --  abc/de ; Head = fg
   function String_Head_Reverse
     (S         : String;
      Separator : Character := '/')
      return      String;

   ------------------------------------------------------------------------
   -- String_Tail_Reverse                                                --
   ------------------------------------------------------------------------
   function String_Tail_Reverse
     (S         : String;
      Separator : Character := '/')
      return      String;

   ------------------------------------------------------------------------
   -- Reverse_String                                                     --
   ------------------------------------------------------------------------
   --  Reverses a string:
   function Reverse_String (S : String) return String;

   function Contains_Word (S : String; W : String; Separator : Character := ' ')
                           return Boolean;
   --  Simple word check

end Agpl.Strings.Fields;
