package body Tinyaml.Nodes.Sequence is

   procedure Append (N : in out Sequence_Node; Item : Node_Access) is
   begin
      N.Elements.Append (Item);
   end Append;

   function Create (Pos : Source_Span := No_Span) return Node_Access is
   begin
      return new Sequence_Node'(Node_Span => Pos,
                                Elements  => Node_Vectors.Empty_Vector);
   end Create;

   function Element (N : Sequence_Node; Index : Positive) return Node_Access is
   begin
      return N.Elements.Element (Index);
   end Element;

   function Length (N : Sequence_Node) return Natural is
   begin
      return Natural (N.Elements.Length);
   end Length;

end Tinyaml.Nodes.Sequence;
