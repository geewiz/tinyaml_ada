with Ada.Strings.Unbounded;
with Tinyaml.Lexer;
with Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Scalar;
with Tinyaml.Nodes.Sequence;

package body Tinyaml.Parser is

   use Ada.Strings.Unbounded;
   use Tinyaml.Lexer;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Map;
   use Tinyaml.Nodes.Sequence;

   --  Forward declarations

   function Parse_Block_Mapping
     (L           : in out Lexer_State;
      First_Key   : Unbounded_String;
      First_Value : Node_Access;
      Start_Span  : Source_Span) return Node_Access;

   function Parse_Block_Sequence
     (L : in out Lexer_State) return Node_Access;

   function Parse_Value
     (L : in out Lexer_State) return Node_Access;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Skip_Newlines (L : in out Lexer_State; T : out Token);
   --  Consume consecutive newline tokens, return first non-newline in T

   function Parse_Value_After_Colon
     (L : in out Lexer_State) return Node_Access;
   --  Parse the value following a colon: inline scalar, nested block, or empty

   function Parse_Sequence_Item (L : in out Lexer_State) return Node_Access;
   --  Parse a single sequence item after the dash

   function Parse_Indented_Value (L : in out Lexer_State) return Node_Access;
   --  Parse value in an indented block (sequence or mapping)

   ---------------------------------------------------------------------------
   --  Helper Implementations
   ---------------------------------------------------------------------------

   procedure Skip_Newlines (L : in out Lexer_State; T : out Token) is
   begin
      Peek_Token (L, T);
      while T.Kind = Token_Newline loop
         Next_Token (L, T);
         Peek_Token (L, T);
      end loop;
   end Skip_Newlines;

   function Parse_Value_After_Colon (L : in out Lexer_State) return Node_Access
   is
      T : Token;
   begin
      Peek_Token (L, T);

      if T.Kind = Token_Newline then
         Next_Token (L, T);
         Peek_Token (L, T);
         if T.Kind = Token_Indent then
            return Parse_Value (L);
         elsif T.Kind = Token_Scalar or else T.Kind = Token_Dash then
            return Parse_Value (L);
         else
            return Scalar.Create ("", No_Span);
         end if;
      elsif T.Kind = Token_Scalar then
         Next_Token (L, T);
         return Scalar.Create (To_String (T.Value), T.Span);
      elsif T.Kind = Token_Indent then
         return Parse_Value (L);
      elsif T.Kind = Token_Dash then
         return Parse_Value (L);
      else
         return Scalar.Create ("", No_Span);
      end if;
   end Parse_Value_After_Colon;

   function Parse_Sequence_Item (L : in out Lexer_State) return Node_Access is
      T : Token;
   begin
      Peek_Token (L, T);

      if T.Kind = Token_Scalar then
         --  Check if this is a mapping (scalar followed by colon)
         declare
            Scalar_T : Token;
            Next_T   : Token;
         begin
            Next_Token (L, Scalar_T);
            Peek_Token (L, Next_T);

            if Next_T.Kind = Token_Colon then
               --  This is a mapping within the sequence
               Next_Token (L, Next_T);  --  Consume colon
               declare
                  Val : constant Node_Access := Parse_Value_After_Colon (L);
               begin
                  return Parse_Block_Mapping
                    (L, Scalar_T.Value, Val, Scalar_T.Span);
               end;
            else
               --  Just a scalar item
               return Scalar.Create
                 (To_String (Scalar_T.Value), Scalar_T.Span);
            end if;
         end;

      elsif T.Kind = Token_Indent then
         return Parse_Value (L);

      elsif T.Kind = Token_Newline then
         Next_Token (L, T);
         Peek_Token (L, T);
         if T.Kind = Token_Indent then
            return Parse_Value (L);
         else
            return Scalar.Create ("", No_Span);
         end if;

      elsif T.Kind = Token_Dash then
         return Parse_Block_Sequence (L);

      else
         return Scalar.Create ("", No_Span);
      end if;
   end Parse_Sequence_Item;

   function Parse_Indented_Value (L : in out Lexer_State) return Node_Access is
      T : Token;
   begin
      Next_Token (L, T);  --  Consume indent
      Peek_Token (L, T);

      if T.Kind = Token_Dash then
         --  Block sequence
         declare
            Seq : constant Node_Access := Parse_Block_Sequence (L);
         begin
            Peek_Token (L, T);
            if T.Kind = Token_Dedent then
               Next_Token (L, T);
            end if;
            return Seq;
         end;

      elsif T.Kind = Token_Scalar then
         --  Might be a mapping - check for colon after scalar
         declare
            Key_Token : Token;
            Next_T    : Token;
         begin
            Next_Token (L, Key_Token);
            Peek_Token (L, Next_T);

            if Next_T.Kind = Token_Colon then
               --  Block mapping
               Next_Token (L, Next_T);  --  Consume colon
               declare
                  Val : constant Node_Access := Parse_Value_After_Colon (L);
                  Map : constant Node_Access := Parse_Block_Mapping
                    (L, Key_Token.Value, Val, Key_Token.Span);
               begin
                  Peek_Token (L, Next_T);
                  if Next_T.Kind = Token_Dedent then
                     Next_Token (L, Next_T);
                  end if;
                  return Map;
               end;
            else
               --  Just a scalar
               return Scalar.Create
                 (To_String (Key_Token.Value), Key_Token.Span);
            end if;
         end;

      else
         raise Parse_Error with
           "Unexpected token after indent: " & T.Kind'Image &
           " at " & Image (T.Span.First);
      end if;
   end Parse_Indented_Value;

   ---------------------------------------------------------------------------
   --  Main Parser Implementation
   ---------------------------------------------------------------------------

   function Parse (Input : String) return Node_Access is
      L : Lexer_State := Create (Input);
      T : Token;
   begin
      if Input'Length = 0 then
         return Scalar.Create ("", No_Span);
      end if;

      Skip_Newlines (L, T);

      if T.Kind = Token_EOF then
         return Scalar.Create ("", No_Span);

      elsif T.Kind = Token_Dash then
         return Parse_Block_Sequence (L);

      elsif T.Kind = Token_Scalar then
         declare
            Key_Token : Token;
            Next_T    : Token;
         begin
            Next_Token (L, Key_Token);
            Peek_Token (L, Next_T);

            if Next_T.Kind = Token_Colon then
               Next_Token (L, Next_T);  --  Consume colon
               return Parse_Block_Mapping
                 (L, Key_Token.Value, Parse_Value_After_Colon (L),
                  Key_Token.Span);
            else
               return Scalar.Create
                 (To_String (Key_Token.Value), Key_Token.Span);
            end if;
         end;

      else
         raise Parse_Error with
           "Unexpected token at start of document: " & T.Kind'Image;
      end if;
   end Parse;

   function Parse_Block_Mapping
     (L           : in out Lexer_State;
      First_Key   : Unbounded_String;
      First_Value : Node_Access;
      Start_Span  : Source_Span) return Node_Access
   is
      pragma Unreferenced (Start_Span);
      Result : constant Node_Access := Map.Create (No_Span);
      T      : Token;
   begin
      Map_Node (Result.all).Insert (To_String (First_Key), First_Value);

      loop
         Skip_Newlines (L, T);
         exit when T.Kind /= Token_Scalar;

         declare
            Key_Token : Token;
            Next_T    : Token;
         begin
            Next_Token (L, Key_Token);
            Peek_Token (L, Next_T);

            --  Must be followed by colon for it to be a mapping key
            exit when Next_T.Kind /= Token_Colon;

            Next_Token (L, Next_T);  --  Consume colon
            Map_Node (Result.all).Insert
              (To_String (Key_Token.Value), Parse_Value_After_Colon (L));
         end;
      end loop;

      return Result;
   end Parse_Block_Mapping;

   function Parse_Block_Sequence
     (L : in out Lexer_State) return Node_Access
   is
      Seq : constant Node_Access := Sequence.Create (No_Span);
      T   : Token;
   begin
      loop
         Peek_Token (L, T);
         exit when T.Kind /= Token_Dash;

         Next_Token (L, T);  --  Consume dash
         Sequence_Node (Seq.all).Append (Parse_Sequence_Item (L));

         --  Skip trailing newline
         Peek_Token (L, T);
         if T.Kind = Token_Newline then
            Next_Token (L, T);
         end if;

         --  Check for dedent (end of sequence) or more items
         Peek_Token (L, T);
         exit when T.Kind = Token_Dedent or else T.Kind = Token_EOF;
      end loop;

      return Seq;
   end Parse_Block_Sequence;

   function Parse_Value
     (L : in out Lexer_State) return Node_Access
   is
      T : Token;
   begin
      Peek_Token (L, T);

      case T.Kind is
         when Token_Scalar =>
            Next_Token (L, T);
            return Scalar.Create (To_String (T.Value), T.Span);

         when Token_Dash =>
            return Parse_Block_Sequence (L);

         when Token_Indent =>
            return Parse_Indented_Value (L);

         when Token_Newline =>
            Next_Token (L, T);
            return Parse_Value (L);

         when Token_EOF | Token_Dedent =>
            return Scalar.Create ("", No_Span);

         when others =>
            raise Parse_Error with
              "Unexpected token: " & T.Kind'Image &
              " at " & Image (T.Span.First);
      end case;
   end Parse_Value;

   function Parse_Document (Input : String) return Documents.Document is
   begin
      return Doc : Documents.Document do
         Documents.Set_Root (Doc, Parse (Input));
      end return;
   end Parse_Document;

end Tinyaml.Parser;
