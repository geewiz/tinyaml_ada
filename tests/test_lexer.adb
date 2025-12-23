with Ada.Strings.Unbounded;

with Test_Harness;
with Tinyaml;
with Tinyaml.Lexer;

package body Test_Lexer is

   use Ada.Strings.Unbounded;
   use Test_Harness;
   use Tinyaml.Lexer;

   procedure Test_Empty_Input is
      L : Lexer_State := Create ("");
      T : Token;
   begin
      Start_Test ("Empty input returns EOF");
      Next_Token (L, T);
      Assert (T.Kind = Token_EOF, "Expected EOF token");
      Pass;
   end Test_Empty_Input;

   procedure Test_Simple_Scalar is
      L : Lexer_State := Create ("hello");
      T : Token;
   begin
      Start_Test ("Simple unquoted scalar");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected Scalar token");
      Assert_Equal ("hello", To_String (T.Value));
      Next_Token (L, T);
      Assert (T.Kind = Token_EOF, "Expected EOF after scalar");
      Pass;
   end Test_Simple_Scalar;

   procedure Test_Quoted_String_Double is
      L : Lexer_State := Create ("""hello world""");
      T : Token;
   begin
      Start_Test ("Double-quoted string");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected Scalar token");
      Assert_Equal ("hello world", To_String (T.Value));
      Pass;
   end Test_Quoted_String_Double;

   procedure Test_Quoted_String_Single is
      L : Lexer_State := Create ("'hello world'");
      T : Token;
   begin
      Start_Test ("Single-quoted string");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected Scalar token");
      Assert_Equal ("hello world", To_String (T.Value));
      Pass;
   end Test_Quoted_String_Single;

   procedure Test_Escape_Sequences is
      L : Lexer_State := Create ("""line1\nline2\ttab""");
      T : Token;
   begin
      Start_Test ("Escape sequences in double-quoted string");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected Scalar token");
      Assert_Equal ("line1" & ASCII.LF & "line2" & ASCII.HT & "tab",
                    To_String (T.Value));
      Pass;
   end Test_Escape_Sequences;

   procedure Test_Colon is
      L : Lexer_State := Create ("key: value");
      T : Token;
   begin
      Start_Test ("Colon token");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected key scalar");
      Assert_Equal ("key", To_String (T.Value));
      Next_Token (L, T);
      Assert (T.Kind = Token_Colon, "Expected Colon token");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected value scalar");
      Assert_Equal ("value", To_String (T.Value));
      Pass;
   end Test_Colon;

   procedure Test_Dash is
      L : Lexer_State := Create ("- item");
      T : Token;
   begin
      Start_Test ("Dash token (sequence item)");
      Next_Token (L, T);
      Assert (T.Kind = Token_Dash, "Expected Dash token");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected item scalar");
      Assert_Equal ("item", To_String (T.Value));
      Pass;
   end Test_Dash;

   procedure Test_Indentation is
      L : Lexer_State := Create ("parent:" & ASCII.LF & "  child: value");
      T : Token;
   begin
      Start_Test ("Indentation tracking");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected parent scalar");
      Assert_Equal ("parent", To_String (T.Value));
      Next_Token (L, T);
      Assert (T.Kind = Token_Colon, "Expected Colon");
      Next_Token (L, T);
      Assert (T.Kind = Token_Newline, "Expected Newline");
      Next_Token (L, T);
      Assert (T.Kind = Token_Indent, "Expected Indent token");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected child scalar");
      Assert_Equal ("child", To_String (T.Value));
      Pass;
   end Test_Indentation;

   procedure Test_Dedent is
      Input : constant String :=
        "parent:" & ASCII.LF &
        "  child: value" & ASCII.LF &
        "sibling: other";
      L : Lexer_State := Create (Input);
      T : Token;
   begin
      Start_Test ("Dedent tracking");
      --  Skip to the dedent
      loop
         Next_Token (L, T);
         exit when T.Kind = Token_Dedent or T.Kind = Token_EOF;
      end loop;
      Assert (T.Kind = Token_Dedent, "Expected Dedent token");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected sibling scalar");
      Assert_Equal ("sibling", To_String (T.Value));
      Pass;
   end Test_Dedent;

   procedure Test_Comment is
      L : Lexer_State := Create ("value # this is a comment");
      T : Token;
   begin
      Start_Test ("Comment handling");
      Next_Token (L, T);
      Assert (T.Kind = Token_Scalar, "Expected scalar");
      Assert_Equal ("value", To_String (T.Value));
      --  Comment should be skipped, next is newline or EOF
      Next_Token (L, T);
      Assert (T.Kind = Token_Newline or T.Kind = Token_EOF,
              "Expected Newline or EOF after comment");
      Pass;
   end Test_Comment;

   procedure Test_Flow_Style_Rejected is
      L : Lexer_State := Create ("[a, b, c]");
      T : Token;
   begin
      Start_Test ("Flow style syntax rejected");
      begin
         Next_Token (L, T);
         Fail ("Should have raised Parse_Error for flow style");
      exception
         when Tinyaml.Parse_Error =>
            Pass;
      end;
   end Test_Flow_Style_Rejected;

   procedure Test_Anchor_Rejected is
      L : Lexer_State := Create ("&anchor value");
      T : Token;
   begin
      Start_Test ("Anchor syntax rejected");
      begin
         Next_Token (L, T);
         Fail ("Should have raised Parse_Error for anchor");
      exception
         when Tinyaml.Parse_Error =>
            Pass;
      end;
   end Test_Anchor_Rejected;

   procedure Test_Tag_Rejected is
      L : Lexer_State := Create ("!tag value");
      T : Token;
   begin
      Start_Test ("Tag syntax rejected");
      begin
         Next_Token (L, T);
         Fail ("Should have raised Parse_Error for tag");
      exception
         when Tinyaml.Parse_Error =>
            Pass;
      end;
   end Test_Tag_Rejected;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Initialize ("Lexer Tests");

      Test_Empty_Input;
      Test_Simple_Scalar;
      Test_Quoted_String_Double;
      Test_Quoted_String_Single;
      Test_Escape_Sequences;
      Test_Colon;
      Test_Dash;
      Test_Indentation;
      Test_Dedent;
      Test_Comment;
      Test_Flow_Style_Rejected;
      Test_Anchor_Rejected;
      Test_Tag_Rejected;
   end Run_Tests;

end Test_Lexer;
