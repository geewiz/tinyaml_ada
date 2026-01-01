with Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;

with Tinyaml.Lexer; use Tinyaml.Lexer;

package body Tinyaml.Lexer_Tests is

   use Ada.Strings.Unbounded;
   use AUnit.Test_Cases.Registration;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Lexer");
   end Name;

   overriding procedure Register_Tests (T : in out Test) is
   begin
      Register_Routine (T, Test_Empty_Input'Access, "Empty input returns EOF");
      Register_Routine (T, Test_Simple_Scalar'Access, "Simple unquoted scalar");
      Register_Routine
        (T, Test_Quoted_String_Double'Access, "Double-quoted string");
      Register_Routine
        (T, Test_Quoted_String_Single'Access, "Single-quoted string");
      Register_Routine
        (T, Test_Escape_Sequences'Access,
         "Escape sequences in double-quoted string");
      Register_Routine (T, Test_Colon'Access, "Colon token");
      Register_Routine (T, Test_Dash'Access, "Dash token (sequence item)");
      Register_Routine (T, Test_Indentation'Access, "Indentation tracking");
      Register_Routine (T, Test_Dedent'Access, "Dedent tracking");
      Register_Routine (T, Test_Comment'Access, "Comment handling");
      Register_Routine
        (T, Test_Flow_Style_Rejected'Access, "Flow style syntax rejected");
      Register_Routine
        (T, Test_Anchor_Rejected'Access, "Anchor syntax rejected");
      Register_Routine (T, Test_Tag_Rejected'Access, "Tag syntax rejected");
   end Register_Tests;

   procedure Test_Empty_Input (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_EOF, "Expected EOF token");
   end Test_Empty_Input;

   procedure Test_Simple_Scalar (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("hello");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected Scalar token");
      Assert (To_String (Tok.Value) = "hello", "Expected 'hello'");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_EOF, "Expected EOF after scalar");
   end Test_Simple_Scalar;

   procedure Test_Quoted_String_Double (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("""hello world""");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected Scalar token");
      Assert (To_String (Tok.Value) = "hello world", "Expected 'hello world'");
   end Test_Quoted_String_Double;

   procedure Test_Quoted_String_Single (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("'hello world'");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected Scalar token");
      Assert (To_String (Tok.Value) = "hello world", "Expected 'hello world'");
   end Test_Quoted_String_Single;

   procedure Test_Escape_Sequences (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L        : Lexer_State := Create ("""line1\nline2\ttab""");
      Tok      : Token;
      Expected : constant String :=
        "line1" & ASCII.LF & "line2" & ASCII.HT & "tab";
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected Scalar token");
      Assert (To_String (Tok.Value) = Expected, "Escape sequences mismatch");
   end Test_Escape_Sequences;

   procedure Test_Colon (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("key: value");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected key scalar");
      Assert (To_String (Tok.Value) = "key", "Expected 'key'");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Colon, "Expected Colon token");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected value scalar");
      Assert (To_String (Tok.Value) = "value", "Expected 'value'");
   end Test_Colon;

   procedure Test_Dash (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("- item");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Dash, "Expected Dash token");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected item scalar");
      Assert (To_String (Tok.Value) = "item", "Expected 'item'");
   end Test_Dash;

   procedure Test_Indentation (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("parent:" & ASCII.LF & "  child: value");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected parent scalar");
      Assert (To_String (Tok.Value) = "parent", "Expected 'parent'");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Colon, "Expected Colon");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Newline, "Expected Newline");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Indent, "Expected Indent token");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected child scalar");
      Assert (To_String (Tok.Value) = "child", "Expected 'child'");
   end Test_Indentation;

   procedure Test_Dedent (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "parent:" & ASCII.LF & "  child: value" & ASCII.LF & "sibling: other";
      L   : Lexer_State := Create (Input);
      Tok : Token;
   begin
      loop
         Next_Token (L, Tok);
         exit when Tok.Kind = Token_Dedent or Tok.Kind = Token_EOF;
      end loop;
      Assert (Tok.Kind = Token_Dedent, "Expected Dedent token");
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected sibling scalar");
      Assert (To_String (Tok.Value) = "sibling", "Expected 'sibling'");
   end Test_Dedent;

   procedure Test_Comment (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("value # this is a comment");
      Tok : Token;
   begin
      Next_Token (L, Tok);
      Assert (Tok.Kind = Token_Scalar, "Expected scalar");
      Assert (To_String (Tok.Value) = "value", "Expected 'value'");
      Next_Token (L, Tok);
      Assert
        (Tok.Kind = Token_Newline or Tok.Kind = Token_EOF,
         "Expected Newline or EOF after comment");
   end Test_Comment;

   procedure Test_Flow_Style_Rejected (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("[a, b, c]");
      Tok : Token;
   begin
      begin
         Next_Token (L, Tok);
         Assert (False, "Should have raised Parse_Error for flow style");
      exception
         when Tinyaml.Parse_Error =>
            null;
      end;
   end Test_Flow_Style_Rejected;

   procedure Test_Anchor_Rejected (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("&anchor value");
      Tok : Token;
   begin
      begin
         Next_Token (L, Tok);
         Assert (False, "Should have raised Parse_Error for anchor");
      exception
         when Tinyaml.Parse_Error =>
            null;
      end;
   end Test_Anchor_Rejected;

   procedure Test_Tag_Rejected (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      L   : Lexer_State := Create ("!tag value");
      Tok : Token;
   begin
      begin
         Next_Token (L, Tok);
         Assert (False, "Should have raised Parse_Error for tag");
      exception
         when Tinyaml.Parse_Error =>
            null;
      end;
   end Test_Tag_Rejected;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (new Test);
      return Result;
   end Suite;

end Tinyaml.Lexer_Tests;
