--  TinyAML Node Types - Document tree representation
--

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Strings.Unbounded;

package Tinyaml.Nodes is

   ---------------------------------------------------------------------------
   --  Abstract Node Base Type
   ---------------------------------------------------------------------------

   type YAML_Node is abstract tagged private;
   type Node_Access is access all YAML_Node'Class;

   --  Node kind enumeration for type checking
   type Node_Kind is (Scalar_Kind, Sequence_Kind, Map_Kind);

   --  Common operations on all nodes
   function Span (N : YAML_Node) return Source_Span;
   procedure Set_Span (N : in out YAML_Node; S : Source_Span);
   function Kind (N : YAML_Node) return Node_Kind is abstract;

   --  Type checking
   function Is_Scalar (N : YAML_Node'Class) return Boolean;
   function Is_Sequence (N : YAML_Node'Class) return Boolean;
   function Is_Map (N : YAML_Node'Class) return Boolean;

   ---------------------------------------------------------------------------
   --  Common Types
   ---------------------------------------------------------------------------

   type String_Array is
     array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------
   --  Container Instantiations (used by child packages)
   ---------------------------------------------------------------------------

   package Node_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Access);

   package String_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Node_Access);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

private

   type YAML_Node is abstract tagged record
      Node_Span : Source_Span := No_Span;
   end record;

end Tinyaml.Nodes;
