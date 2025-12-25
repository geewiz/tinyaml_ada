--  TinyAML Lexer - Tokenizes YAML input strings
--

with Ada.Strings.Unbounded;

package Tinyaml.Lexer is

   --  Token types
   type Token_Kind is
     (Token_EOF,           --  End of input
      Token_Newline,       --  Line break
      Token_Indent,        --  Indentation increased
      Token_Dedent,        --  Indentation decreased
      Token_Colon,         --  ':'
      Token_Dash,          --  '-' at start of line (sequence item)
      Token_Pipe,          --  '|' (literal block scalar)
      Token_Greater,       --  '>' (folded block scalar)
      Token_Scalar);       --  String value (quoted or unquoted)

   --  A single token with its value and source location
   type Token is record
      Kind  : Token_Kind := Token_EOF;
      Value : Ada.Strings.Unbounded.Unbounded_String;
      Span  : Source_Span := No_Span;
   end record;

   --  Lexer state (opaque to callers)
   type Lexer_State is private;

   --  Create a new lexer for the given input string.
   --  The input string must remain valid for the lifetime of the lexer.
   function Create (Input : String) return Lexer_State;

   --  Get the next token from the input.
   --  Returns Token_EOF when input is exhausted.
   --  Raises Parse_Error on invalid input.
   procedure Next_Token (L : in out Lexer_State; T : out Token);

   --  Peek at the next token without consuming it.
   procedure Peek_Token (L : in out Lexer_State; T : out Token);

   --  Check if the lexer has reached end of input
   function At_End (L : Lexer_State) return Boolean;

   --  Get current position in the input (for error messages)
   function Current_Position (L : Lexer_State) return Source_Position;

private

   --  Maximum nesting depth for indentation
   Max_Indent_Depth : constant := 100;

   --  Indentation stack
   type Indent_Stack is array (1 .. Max_Indent_Depth) of Natural;

   type Lexer_State is record
      --  Input string (access to original)
      Input  : Ada.Strings.Unbounded.Unbounded_String;
      Length : Natural := 0;

      --  Current position
      Pos    : Positive := 1;  --  Current character index (1-based)
      Line   : Positive := 1;  --  Current line number
      Column : Positive := 1;  --  Current column number

      --  Indentation tracking
      Indents     : Indent_Stack := [others => 0];
      Indent_Top  : Natural := 1;  --  Stack top (1 = base level 0)
      Line_Indent : Natural := 0;  --  Indentation of current line

      --  Token lookahead
      Has_Peeked   : Boolean := False;
      Peeked_Token : Token;

      --  State flags
      At_Line_Start   : Boolean := True;   --  At start of a line?
      Pending_Dedents : Natural := 0;      --  Dedents to emit next
      Pending_Newline : Boolean := False;  --  Newline before dedents
   end record;

end Tinyaml.Lexer;
