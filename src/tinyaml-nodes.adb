package body Tinyaml.Nodes is

   function Is_Map (N : YAML_Node'Class) return Boolean is
   begin
      return N.Kind = Map_Kind;
   end Is_Map;

   function Is_Scalar (N : YAML_Node'Class) return Boolean is
   begin
      return N.Kind = Scalar_Kind;
   end Is_Scalar;

   function Is_Sequence (N : YAML_Node'Class) return Boolean is
   begin
      return N.Kind = Sequence_Kind;
   end Is_Sequence;

   procedure Set_Span (N : in out YAML_Node; S : Source_Span) is
   begin
      N.Node_Span := S;
   end Set_Span;

   function Span (N : YAML_Node) return Source_Span is
   begin
      return N.Node_Span;
   end Span;

end Tinyaml.Nodes;
