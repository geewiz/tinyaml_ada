--  TinyAML Validation - Schema validation and type-safe access
--

with Ada.Strings.Unbounded;

with Tinyaml.Nodes;
with Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Scalar;
with Tinyaml.Nodes.Sequence;
with Tinyaml.Schemas;

package Tinyaml.Validation is

   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Map;
   use Tinyaml.Nodes.Scalar;
   use Tinyaml.Nodes.Sequence;
   use Tinyaml.Schemas;

   ---------------------------------------------------------------------------
   --  Validated Node Types
   ---------------------------------------------------------------------------

   --  Validated_Scalar: A scalar that has been validated and can be
   --  converted to typed values.
   type Validated_Scalar is new Scalar_Node with private;

   --  Type-safe accessors (raise Validation_Error on conversion failure)
   function As_Integer (N : Validated_Scalar) return Integer;
   function As_Float (N : Validated_Scalar) return Long_Float;
   function As_Boolean (N : Validated_Scalar) return Boolean;

   --  Validated_Sequence: A sequence that has been validated
   type Validated_Sequence is new Sequence_Node with private;

   --  Validated_Map: A map that has been validated
   type Validated_Map is new Map_Node with private;

   ---------------------------------------------------------------------------
   --  Validation Result
   ---------------------------------------------------------------------------

   --  Validation_Result: Holds the result of validation, including
   --  any errors that occurred.
   type Validation_Result is tagged private;

   --  Check if validation succeeded
   function Is_Valid (R : Validation_Result) return Boolean;

   --  Get the validated root node (raises Validation_Error if not valid)
   function Root (R : Validation_Result) return Node_Access;

   --  Get error message if validation failed
   function Error_Message (R : Validation_Result) return String;

   --  Get the path where validation failed
   function Error_Path (R : Validation_Result) return String;

   --  Get the offending value that caused validation to fail
   function Error_Value (R : Validation_Result) return String;

   ---------------------------------------------------------------------------
   --  Validation Functions
   ---------------------------------------------------------------------------

   --  Validate a node against a schema.
   --  Returns a Validation_Result that can be queried.
   function Validate
     (Node   : Node_Access;
      Schema : Schemas.Schema'Class) return Validation_Result;

   --  Validate and raise Validation_Error on failure.
   --  Returns the validated node on success.
   function Validate_Or_Raise
     (Node   : Node_Access;
      Schema : Schemas.Schema'Class) return Node_Access;

   ---------------------------------------------------------------------------
   --  Convenience Functions
   ---------------------------------------------------------------------------

   --  Parse and validate in one step
   function Parse_And_Validate
     (Input  : String;
      Schema : Schemas.Schema'Class) return Validation_Result;

   --  Parse, validate, and raise on failure
   function Parse_And_Validate_Or_Raise
     (Input  : String;
      Schema : Schemas.Schema'Class) return Node_Access;

private

   type Validated_Scalar is new Scalar_Node with null record;
   type Validated_Sequence is new Sequence_Node with null record;
   type Validated_Map is new Map_Node with null record;

   type Validation_Result is tagged record
      Valid   : Boolean := False;
      Result  : Node_Access := null;
      Message : Ada.Strings.Unbounded.Unbounded_String;
      Path    : Ada.Strings.Unbounded.Unbounded_String;
      Value   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Tinyaml.Validation;
