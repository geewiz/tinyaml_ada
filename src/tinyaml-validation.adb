with Ada.Characters.Handling;
with Ada.Exceptions;

with Tinyaml.Parser;
with Tinyaml.Nodes.Scalar; use Tinyaml.Nodes.Scalar;

package body Tinyaml.Validation is

   use Ada.Strings.Unbounded;

   function As_Boolean (N : Validated_Scalar) return Boolean is
      Val   : constant String := Scalar_Node (N).Value;
      Lower : constant String := Ada.Characters.Handling.To_Lower (Val);
   begin
      if Lower = "true" or else Lower = "yes" or else Lower = "on" then
         return True;
      elsif Lower = "false" or else Lower = "no" or else Lower = "off" then
         return False;
      else
         raise Validation_Error with
           "Cannot convert '" & Val & "' to boolean";
      end if;
   end As_Boolean;

   function As_Float (N : Validated_Scalar) return Long_Float is
      Val : constant String := Scalar_Node (N).Value;
   begin
      return Long_Float'Value (Val);
   exception
      when Constraint_Error =>
         raise Validation_Error with
           "Cannot convert '" & Val & "' to float";
   end As_Float;

   function As_Integer (N : Validated_Scalar) return Integer is
      Val : constant String := Scalar_Node (N).Value;
   begin
      return Integer'Value (Val);
   exception
      when Constraint_Error =>
         raise Validation_Error with
           "Cannot convert '" & Val & "' to integer";
   end As_Integer;

   function Error_Message (R : Validation_Result) return String is
   begin
      return To_String (R.Message);
   end Error_Message;

   function Error_Path (R : Validation_Result) return String is
   begin
      return To_String (R.Path);
   end Error_Path;

   function Error_Value (R : Validation_Result) return String is
   begin
      return To_String (R.Value);
   end Error_Value;

   function Is_Valid (R : Validation_Result) return Boolean is
   begin
      return R.Valid;
   end Is_Valid;

   function Parse_And_Validate
     (Input  : String;
      Schema : Schemas.Schema'Class) return Validation_Result
   is
      Node : Node_Access;
   begin
      Node := Parser.Parse (Input);
      return Validate (Node, Schema);
   exception
      when E : Parse_Error =>
         return Validation_Result'
           (Valid   => False,
            Result  => null,
            Message => To_Unbounded_String
              (Ada.Exceptions.Exception_Message (E)),
            Path    => Null_Unbounded_String,
            Value   => Null_Unbounded_String);
   end Parse_And_Validate;

   function Parse_And_Validate_Or_Raise
     (Input  : String;
      Schema : Schemas.Schema'Class) return Node_Access
   is
      Node : constant Node_Access := Parser.Parse (Input);
   begin
      return Validate_Or_Raise (Node, Schema);
   end Parse_And_Validate_Or_Raise;

   function Root (R : Validation_Result) return Node_Access is
   begin
      if not R.Valid then
         raise Validation_Error with To_String (R.Message);
      end if;
      return R.Result;
   end Root;

   function Validate
     (Node   : Node_Access;
      Schema : Schemas.Schema'Class) return Validation_Result
   is
      Info : constant Schemas.Validation_Info := Schema.Validate_Node (Node);
   begin
      if Info.Is_Valid then
         return Validation_Result'
           (Valid   => True,
            Result  => Node,
            Message => Null_Unbounded_String,
            Path    => Null_Unbounded_String,
            Value   => Null_Unbounded_String);
      else
         return Validation_Result'
           (Valid   => False,
            Result  => null,
            Message => Info.Message,
            Path    => Info.Path,
            Value   => Info.Value);
      end if;
   end Validate;

   function Validate_Or_Raise
     (Node   : Node_Access;
      Schema : Schemas.Schema'Class) return Node_Access
   is
      R : constant Validation_Result := Validate (Node, Schema);
   begin
      if not R.Valid then
         raise Validation_Error with To_String (R.Message);
      end if;
      return R.Result;
   end Validate_Or_Raise;

end Tinyaml.Validation;
