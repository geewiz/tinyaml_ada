--  Sequence_Node: An ordered list of child nodes

package Tinyaml.Nodes.Sequence is

   type Sequence_Node is new YAML_Node with private;

   --  Create a new empty sequence node
   function Create (Pos : Source_Span := No_Span) return Node_Access;

   --  Get number of elements
   function Length (N : Sequence_Node) return Natural;

   --  Get element at index (1-based)
   function Element (N : Sequence_Node; Index : Positive) return Node_Access
     with Pre => Index <= N.Length;

   --  Append an element
   procedure Append (N : in out Sequence_Node; Item : Node_Access);

   --  Node kind
   overriding function Kind (N : Sequence_Node) return Node_Kind
     is (Sequence_Kind);

private

   type Sequence_Node is new YAML_Node with record
      Elements : Node_Vectors.Vector;
   end record;

end Tinyaml.Nodes.Sequence;
