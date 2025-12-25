with Ada.Unchecked_Deallocation;

with Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Sequence;

package body Tinyaml.Nodes is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => YAML_Node'Class,
      Name   => Node_Access);

   procedure Free_Sequence_Children (Seq : Sequence.Sequence_Node);
   procedure Free_Map_Children (M : Map.Map_Node);

   procedure Free_Sequence_Children (Seq : Sequence.Sequence_Node) is
   begin
      for I in 1 .. Seq.Length loop
         declare
            Child : Node_Access := Seq.Element (I);
         begin
            Free_Node (Child);
         end;
      end loop;
   end Free_Sequence_Children;

   procedure Free_Map_Children (M : Map.Map_Node) is
      use Ada.Strings.Unbounded;
      Keys : constant String_Array := M.Keys;
   begin
      for K of Keys loop
         declare
            Child : Node_Access := M.Get (To_String (K));
         begin
            Free_Node (Child);
         end;
      end loop;
   end Free_Map_Children;

   procedure Free_Node (N : in out Node_Access) is
   begin
      if N = null then
         return;
      end if;

      case N.Kind is
         when Scalar_Kind =>
            null;
         when Sequence_Kind =>
            Free_Sequence_Children (Sequence.Sequence_Node (N.all));
         when Map_Kind =>
            Free_Map_Children (Map.Map_Node (N.all));
      end case;

      Deallocate (N);
   end Free_Node;

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
