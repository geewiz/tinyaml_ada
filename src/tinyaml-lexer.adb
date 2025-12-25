with Ada.Characters.Latin_1;

package body Tinyaml.Lexer is

   use Ada.Strings.Unbounded;
   use Ada.Characters.Latin_1;

   --  Forward declarations for local helpers
   procedure Advance (L : in out Lexer_State);
   function Is_Space (C : Character) return Boolean;
   function Is_Newline (C : Character) return Boolean;
   function Char_At (L : Lexer_State; Offset : Natural := 0) return Character;
   function Make_Position (L : Lexer_State) return Source_Position;
   procedure Skip_Spaces (L : in out Lexer_State);
   procedure Skip_To_EOL (L : in out Lexer_State);
   function Count_Indent (L : Lexer_State) return Natural;
   procedure Check_No_Flow (L : Lexer_State; C : Character);
   procedure Check_No_Anchor (L : Lexer_State; C : Character);
   procedure Check_No_Tag (L : Lexer_State; C : Character);
   procedure Read_Quoted_String
     (L     : in out Lexer_State;
      Quote : Character;
      Value : out Unbounded_String);
   procedure Read_Unquoted_Scalar
     (L     : in out Lexer_State;
      Value : out Unbounded_String);
   procedure Read_Block_Scalar
     (L      : in out Lexer_State;
      Folded : Boolean;
      Value  : out Unbounded_String);

   --  Helper: Advance position by one character
   procedure Advance (L : in out Lexer_State) is
      C : constant Character := Char_At (L);
   begin
      if L.Pos <= L.Length then
         L.Pos := L.Pos + 1;
         if C = LF then
            L.Line := L.Line + 1;
            L.Column := 1;
         elsif C = CR then
            --  Handle CR or CRLF
            if Char_At (L) = LF then
               L.Pos := L.Pos + 1;
            end if;
            L.Line := L.Line + 1;
            L.Column := 1;
         else
            L.Column := L.Column + 1;
         end if;
      end if;
   end Advance;

   function At_End (L : Lexer_State) return Boolean is
     (L.Pos > L.Length and then not L.Has_Peeked
      and then L.Pending_Dedents = 0);

   --  Helper: Get character at position, or NUL if past end
   function Char_At (L : Lexer_State; Offset : Natural := 0) return Character
   is
      Idx : constant Natural := L.Pos + Offset;
   begin
      if Idx <= L.Length then
         return Element (L.Input, Idx);
      else
         return NUL;
      end if;
   end Char_At;

   --  Helper: Raise error for disallowed anchors/aliases
   procedure Check_No_Anchor (L : Lexer_State; C : Character) is
   begin
      if C = '&' or else C = '*' then
         raise Parse_Error with
           "Anchors and aliases not allowed at " & Image (Make_Position (L));
      end if;
   end Check_No_Anchor;

   --  Helper: Raise error for disallowed flow syntax
   procedure Check_No_Flow (L : Lexer_State; C : Character) is
   begin
      if C = '[' or else C = ']' or else C = '{' or else C = '}' then
         raise Parse_Error with
           "Flow style syntax not allowed at " & Image (Make_Position (L));
      end if;
   end Check_No_Flow;

   --  Helper: Raise error for disallowed tags
   procedure Check_No_Tag (L : Lexer_State; C : Character) is
   begin
      if C = '!' then
         raise Parse_Error with
           "Tags not allowed at " & Image (Make_Position (L));
      end if;
   end Check_No_Tag;

   --  Helper: Count leading spaces at current position (for indentation)
   function Count_Indent (L : Lexer_State) return Natural is
      Count : Natural := 0;
      Idx   : Natural := L.Pos;
   begin
      while Idx <= L.Length loop
         declare
            C : constant Character := Element (L.Input, Idx);
         begin
            if C = ' ' then
               Count := Count + 1;
               Idx := Idx + 1;
            elsif C = HT then
               --  Tabs are not allowed for indentation in strict YAML
               raise Parse_Error with
                 "Tab character not allowed for indentation at " &
                 Image (Make_Position (L));
            else
               exit;
            end if;
         end;
      end loop;
      return Count;
   end Count_Indent;

   function Create (Input : String) return Lexer_State is
   begin
      return L : Lexer_State do
         L.Input := To_Unbounded_String (Input);
         L.Length := Input'Length;
         L.Indents (1) := 0;  --  Base indentation level
      end return;
   end Create;

   function Current_Position (L : Lexer_State) return Source_Position is
     (Make_Position (L));

   --  Helper: Check if character is a space or tab
   function Is_Space (C : Character) return Boolean is
     (C = ' ' or else C = HT);

   --  Helper: Check if character is a newline
   function Is_Newline (C : Character) return Boolean is
     (C = LF or else C = CR);

   --  Helper: Make a source position from current state
   function Make_Position (L : Lexer_State) return Source_Position is
     (Line => L.Line, Column => L.Column, Offset => L.Pos - 1);

   procedure Next_Token (L : in out Lexer_State; T : out Token) is
      Start : Source_Position;
   begin
      --  Return peeked token if available
      if L.Has_Peeked then
         T := L.Peeked_Token;
         L.Has_Peeked := False;
         return;
      end if;

      --  Handle pending newline before dedents
      if L.Pending_Newline then
         L.Pending_Newline := False;
         T := Token'(Kind  => Token_Newline,
                     Value => Null_Unbounded_String,
                     Span  => (First => Make_Position (L),
                               Last  => Make_Position (L)));
         return;
      end if;

      --  Handle pending dedents
      if L.Pending_Dedents > 0 then
         L.Pending_Dedents := L.Pending_Dedents - 1;
         T := Token'(Kind  => Token_Dedent,
                     Value => Null_Unbounded_String,
                     Span  => (First => Make_Position (L),
                               Last  => Make_Position (L)));
         return;
      end if;

      --  Handle line start: check indentation
      if L.At_Line_Start then
         L.Line_Indent := Count_Indent (L);

         --  Skip the indentation spaces
         for I in 1 .. L.Line_Indent loop
            Advance (L);
         end loop;

         --  Skip empty lines and comment-only lines
         loop
            declare
               C : constant Character := Char_At (L);
            begin
               if C = '#' then
                  Skip_To_EOL (L);
               end if;

               if Is_Newline (C) then
                  Advance (L);
                  L.Line_Indent := Count_Indent (L);
                  for I in 1 .. L.Line_Indent loop
                     Advance (L);
                  end loop;
               elsif C = NUL then
                  --  End of input: emit dedents for all open indents
                  if L.Indent_Top > 1 then
                     L.Pending_Dedents := L.Indent_Top - 2;
                     L.Indent_Top := 1;
                     T := Token'(Kind  => Token_Dedent,
                                 Value => Null_Unbounded_String,
                                 Span  => (First => Make_Position (L),
                                           Last  => Make_Position (L)));
                     return;
                  end if;
                  T := Token'(Kind  => Token_EOF,
                              Value => Null_Unbounded_String,
                              Span  => (First => Make_Position (L),
                                        Last  => Make_Position (L)));
                  return;
               else
                  exit;
               end if;
            end;
         end loop;

         L.At_Line_Start := False;

         --  Compare with current indent level
         declare
            Current_Indent : constant Natural := L.Indents (L.Indent_Top);
         begin
            if L.Line_Indent > Current_Indent then
               --  Increased indentation: push and emit Indent
               if L.Indent_Top >= Max_Indent_Depth then
                  raise Parse_Error with
                    "Maximum indentation depth exceeded at " &
                    Image (Make_Position (L));
               end if;
               L.Indent_Top := L.Indent_Top + 1;
               L.Indents (L.Indent_Top) := L.Line_Indent;
               T := Token'(Kind  => Token_Indent,
                           Value => Null_Unbounded_String,
                           Span  => (First => Make_Position (L),
                                     Last  => Make_Position (L)));
               return;

            elsif L.Line_Indent < Current_Indent then
               --  Decreased indentation: pop and emit Dedent(s)
               while L.Indent_Top > 1 and then
                 L.Indents (L.Indent_Top) > L.Line_Indent
               loop
                  L.Indent_Top := L.Indent_Top - 1;
               end loop;

               --  Check indent matches a previous level
               if L.Indents (L.Indent_Top) /= L.Line_Indent then
                  raise Parse_Error with
                    "Indentation does not match any outer level at " &
                    Image (Make_Position (L));
               end if;

               --  Calculate how many dedents we need
               declare
                  Dedents_Needed : constant Natural :=
                    (Current_Indent - L.Line_Indent) / 2;  --  Approximate
               begin
                  if Dedents_Needed > 1 then
                     L.Pending_Dedents := Dedents_Needed - 1;
                  end if;
               end;

               T := Token'(Kind  => Token_Dedent,
                           Value => Null_Unbounded_String,
                           Span  => (First => Make_Position (L),
                                     Last  => Make_Position (L)));
               return;
            end if;
         end;
      end if;

      --  Skip spaces within a line
      Skip_Spaces (L);

      Start := Make_Position (L);

      --  Check for end of input
      if Char_At (L) = NUL then
         --  Emit remaining dedents
         if L.Indent_Top > 1 then
            L.Pending_Dedents := L.Indent_Top - 2;
            L.Indent_Top := 1;
            T := Token'(Kind  => Token_Dedent,
                        Value => Null_Unbounded_String,
                        Span  => (First => Start, Last => Start));
            return;
         end if;
         T := Token'(Kind  => Token_EOF,
                     Value => Null_Unbounded_String,
                     Span  => (First => Start, Last => Start));
         return;
      end if;

      --  Check for newline
      if Is_Newline (Char_At (L)) then
         Advance (L);
         L.At_Line_Start := True;
         T := Token'(Kind  => Token_Newline,
                     Value => Null_Unbounded_String,
                     Span  => (First => Start, Last => Make_Position (L)));
         return;
      end if;

      --  Check for comment
      if Char_At (L) = '#' then
         Skip_To_EOL (L);
         if Is_Newline (Char_At (L)) then
            Advance (L);
            L.At_Line_Start := True;
         end if;
         T := Token'(Kind  => Token_Newline,
                     Value => Null_Unbounded_String,
                     Span  => (First => Start, Last => Make_Position (L)));
         return;
      end if;

      --  Check disallowed syntax
      Check_No_Flow (L, Char_At (L));
      Check_No_Anchor (L, Char_At (L));
      Check_No_Tag (L, Char_At (L));

      --  Check for colon
      if Char_At (L) = ':' then
         declare
            Next : constant Character := Char_At (L, 1);
         begin
            if Is_Space (Next) or else Is_Newline (Next) or else
              Next = NUL
            then
               Advance (L);
               T := Token'(Kind  => Token_Colon,
                           Value => Null_Unbounded_String,
                           Span  => (First => Start,
                                     Last  => Make_Position (L)));
               return;
            end if;
         end;
      end if;

      --  Check for dash (sequence item)
      if Char_At (L) = '-' then
         declare
            Next : constant Character := Char_At (L, 1);
         begin
            if Is_Space (Next) or else Is_Newline (Next) or else
              Next = NUL
            then
               Advance (L);
               T := Token'(Kind  => Token_Dash,
                           Value => Null_Unbounded_String,
                           Span  => (First => Start,
                                     Last  => Make_Position (L)));
               return;
            end if;
         end;
      end if;

      --  Check for block scalar indicators
      if Char_At (L) = '|' then
         declare
            Block_Value : Unbounded_String;
         begin
            Read_Block_Scalar (L, Folded => False, Value => Block_Value);
            T := Token'(Kind  => Token_Scalar,
                        Value => Block_Value,
                        Span  => (First => Start, Last => Make_Position (L)));
            L.At_Line_Start := True;
            return;
         end;
      end if;

      if Char_At (L) = '>' then
         declare
            Block_Value : Unbounded_String;
         begin
            Read_Block_Scalar (L, Folded => True, Value => Block_Value);
            T := Token'(Kind  => Token_Scalar,
                        Value => Block_Value,
                        Span  => (First => Start, Last => Make_Position (L)));
            L.At_Line_Start := True;
            return;
         end;
      end if;

      --  Check for quoted strings
      if Char_At (L) = '"' or else Char_At (L) = ''' then
         declare
            Quote       : constant Character := Char_At (L);
            String_Value : Unbounded_String;
         begin
            Read_Quoted_String (L, Quote, String_Value);
            T := Token'(Kind  => Token_Scalar,
                        Value => String_Value,
                        Span  => (First => Start, Last => Make_Position (L)));
            return;
         end;
      end if;

      --  Unquoted scalar
      declare
         Scalar_Value : Unbounded_String;
      begin
         Read_Unquoted_Scalar (L, Scalar_Value);
         T := Token'(Kind  => Token_Scalar,
                     Value => Scalar_Value,
                     Span  => (First => Start, Last => Make_Position (L)));
      end;
   end Next_Token;

   procedure Peek_Token (L : in out Lexer_State; T : out Token) is
   begin
      if L.Has_Peeked then
         T := L.Peeked_Token;
      else
         Next_Token (L, L.Peeked_Token);
         L.Has_Peeked := True;
         T := L.Peeked_Token;
      end if;
   end Peek_Token;

   --  Helper: Read a block scalar (literal | or folded >)
   procedure Read_Block_Scalar
     (L      : in out Lexer_State;
      Folded : Boolean;
      Value  : out Unbounded_String)
   is
      Result       : Unbounded_String;
      Block_Indent : Natural := 0;
      First_Line   : Boolean := True;
   begin
      --  Skip the indicator (| or >)
      Advance (L);

      --  Skip to end of line (ignore chomping indicators for now)
      Skip_Spaces (L);
      if Char_At (L) = '#' then
         Skip_To_EOL (L);
      end if;

      --  Must have newline after indicator
      if not Is_Newline (Char_At (L)) and then Char_At (L) /= NUL then
         raise Parse_Error with
           "Expected newline after block scalar indicator at " &
           Image (Make_Position (L));
      end if;

      --  Consume the newline
      if Is_Newline (Char_At (L)) then
         Advance (L);
      end if;

      --  Read content lines
      loop
         --  Count indentation of this line
         declare
            Line_Indent : constant Natural := Count_Indent (L);
         begin
            --  Skip the spaces
            for I in 1 .. Line_Indent loop
               Advance (L);
            end loop;

            --  Empty line?
            if Is_Newline (Char_At (L)) or else Char_At (L) = NUL then
               if Length (Result) > 0 then
                  Append (Result, LF);
               end if;
               if Is_Newline (Char_At (L)) then
                  Advance (L);
               end if;
               if Char_At (L) = NUL then
                  exit;
               end if;
            elsif First_Line then
               --  First content line determines block indent
               Block_Indent := Line_Indent;
               First_Line := False;

               --  Read the content
               while not Is_Newline (Char_At (L)) and then
                 Char_At (L) /= NUL
               loop
                  Append (Result, Char_At (L));
                  Advance (L);
               end loop;
               if Is_Newline (Char_At (L)) then
                  if not Folded then
                     Append (Result, LF);
                  end if;
                  Advance (L);
               end if;
            elsif Line_Indent >= Block_Indent then
               --  Continuation line
               --  Add extra indentation as content
               for I in Block_Indent + 1 .. Line_Indent loop
                  Append (Result, ' ');
               end loop;

               --  For folded style, add space instead of previous newline
               if Folded and then Length (Result) > 0 and then
                 Element (Result, Length (Result)) /= LF
               then
                  Append (Result, ' ');
               end if;

               --  Read the content
               while not Is_Newline (Char_At (L)) and then
                 Char_At (L) /= NUL
               loop
                  Append (Result, Char_At (L));
                  Advance (L);
               end loop;
               if Is_Newline (Char_At (L)) then
                  if not Folded then
                     Append (Result, LF);
                  end if;
                  Advance (L);
               end if;
            else
               --  Less indented line ends the block
               --  Back up to start of line for proper handling
               L.Pos := L.Pos - Line_Indent;
               L.Column := 1;
               exit;
            end if;
         end;
      end loop;

      --  Trim trailing newline for default chomp
      while Length (Result) > 0 and then
        Element (Result, Length (Result)) = LF
      loop
         Delete (Result, Length (Result), Length (Result));
      end loop;

      Value := Result;
   end Read_Block_Scalar;

   --  Helper: Read a quoted string (single or double)
   procedure Read_Quoted_String
     (L     : in out Lexer_State;
      Quote : Character;
      Value : out Unbounded_String)
   is
      Start_Pos : constant Source_Position := Make_Position (L);
      Result    : Unbounded_String;
   begin
      Advance (L);  --  Skip opening quote

      loop
         declare
            C : constant Character := Char_At (L);
         begin
            if C = NUL then
               raise Parse_Error with
                 "Unterminated string starting at " & Image (Start_Pos);
            elsif C = Quote then
               if Quote = ''' and then Char_At (L, 1) = ''' then
                  --  Escaped single quote in single-quoted string
                  Append (Result, ''');
                  Advance (L);
                  Advance (L);
               else
                  --  End of string
                  Advance (L);
                  exit;
               end if;
            elsif C = '\' and then Quote = '"' then
               --  Escape sequence in double-quoted string
               Advance (L);
               declare
                  Esc : constant Character := Char_At (L);
               begin
                  case Esc is
                     when 'n' => Append (Result, LF);
                     when 'r' => Append (Result, CR);
                     when 't' => Append (Result, HT);
                     when '\' => Append (Result, '\');
                     when '"' => Append (Result, '"');
                     when '/' => Append (Result, '/');
                     when '0' => Append (Result, NUL);
                     when others =>
                        raise Parse_Error with
                          "Invalid escape sequence at " &
                          Image (Make_Position (L));
                  end case;
                  Advance (L);
               end;
            elsif Is_Newline (C) then
               --  Newlines in quoted strings become spaces
               Append (Result, ' ');
               Advance (L);
               Skip_Spaces (L);
            else
               Append (Result, C);
               Advance (L);
            end if;
         end;
      end loop;

      Value := Result;
   end Read_Quoted_String;

   --  Helper: Read an unquoted scalar
   procedure Read_Unquoted_Scalar
     (L     : in out Lexer_State;
      Value : out Unbounded_String)
   is
      Result : Unbounded_String;
   begin
      loop
         declare
            C : constant Character := Char_At (L);
         begin
            exit when C = NUL or else Is_Newline (C);

            --  Check for comment
            if C = '#' and then
              (Length (Result) = 0 or else
               Is_Space (Element (Result, Length (Result))))
            then
               --  Trim trailing space before comment
               if Length (Result) > 0 and then
                 Is_Space (Element (Result, Length (Result)))
               then
                  Delete (Result, Length (Result), Length (Result));
               end if;
               exit;
            end if;

            --  Check for colon followed by space/newline/EOF (mapping value)
            if C = ':' then
               declare
                  Next : constant Character := Char_At (L, 1);
               begin
                  if Is_Space (Next) or else Is_Newline (Next) or else
                    Next = NUL
                  then
                     exit;
                  end if;
               end;
            end if;

            Append (Result, C);
            Advance (L);
         end;
      end loop;

      --  Trim trailing whitespace
      while Length (Result) > 0 and then
        Is_Space (Element (Result, Length (Result)))
      loop
         Delete (Result, Length (Result), Length (Result));
      end loop;

      Value := Result;
   end Read_Unquoted_Scalar;

   --  Helper: Skip whitespace (spaces and tabs only, not newlines)
   procedure Skip_Spaces (L : in out Lexer_State) is
   begin
      while Is_Space (Char_At (L)) loop
         Advance (L);
      end loop;
   end Skip_Spaces;

   --  Helper: Skip to end of line (for comments)
   procedure Skip_To_EOL (L : in out Lexer_State) is
   begin
      while not Is_Newline (Char_At (L)) and then Char_At (L) /= NUL loop
         Advance (L);
      end loop;
   end Skip_To_EOL;

end Tinyaml.Lexer;
