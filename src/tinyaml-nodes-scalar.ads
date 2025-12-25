--  Scalar_Node: A leaf node containing a string value

with Ada.Strings.Unbounded;

package Tinyaml.Nodes.Scalar is

   type Scalar_Node is new YAML_Node with private;

   --  Create a new scalar node
   function Create
     (Value : String;
      Pos   : Source_Span := No_Span) return Node_Access;

   --  Get the string value
   function Value (N : Scalar_Node) return String;
   function Value
     (N : Scalar_Node) return Ada.Strings.Unbounded.Unbounded_String;

   --  Get scalar value from Node_Access (convenience, avoids .all)
   function Value (N : Node_Access) return String;

   overriding function Kind (N : Scalar_Node) return Node_Kind
     is (Scalar_Kind);

private

   type Scalar_Node is new YAML_Node with record
      Str_Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Tinyaml.Nodes.Scalar;
