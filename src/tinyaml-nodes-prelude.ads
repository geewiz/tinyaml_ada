--  Convenience package that re-exports all node types
--
--  Note: Use with Tinyaml.Nodes for base types (Node_Access, Is_Scalar, etc.)

with Ada.Strings.Unbounded;

with Tinyaml.Nodes.Scalar;
with Tinyaml.Nodes.Sequence;
with Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Navigation;

package Tinyaml.Nodes.Prelude is

   ---------------------------------------------------------------------------
   --  Re-export Scalar_Node
   ---------------------------------------------------------------------------

   subtype Scalar_Node is Tinyaml.Nodes.Scalar.Scalar_Node;

   function Create
     (Value : String;
      Pos   : Source_Span := No_Span) return Node_Access
     renames Tinyaml.Nodes.Scalar.Create;

   function Value (N : Scalar_Node) return String
     renames Tinyaml.Nodes.Scalar.Value;

   function Value
     (N : Scalar_Node) return Ada.Strings.Unbounded.Unbounded_String
     renames Tinyaml.Nodes.Scalar.Value;

   function Value (N : Node_Access) return String
     renames Tinyaml.Nodes.Scalar.Value;

   ---------------------------------------------------------------------------
   --  Re-export Sequence_Node
   ---------------------------------------------------------------------------

   subtype Sequence_Node is Tinyaml.Nodes.Sequence.Sequence_Node;

   function Create_Sequence (Pos : Source_Span := No_Span) return Node_Access
     renames Tinyaml.Nodes.Sequence.Create;

   function Length (N : Sequence_Node) return Natural
     renames Tinyaml.Nodes.Sequence.Length;

   function Element (N : Sequence_Node; Index : Positive) return Node_Access
     renames Tinyaml.Nodes.Sequence.Element;

   procedure Append (N : in out Sequence_Node; Item : Node_Access)
     renames Tinyaml.Nodes.Sequence.Append;

   ---------------------------------------------------------------------------
   --  Re-export Map_Node
   ---------------------------------------------------------------------------

   subtype Map_Node is Tinyaml.Nodes.Map.Map_Node;

   function Create_Map (Pos : Source_Span := No_Span) return Node_Access
     renames Tinyaml.Nodes.Map.Create;

   function Contains (N : Map_Node; Key : String) return Boolean
     renames Tinyaml.Nodes.Map.Contains;

   function Get (N : Map_Node; Key : String) return Node_Access
     renames Tinyaml.Nodes.Map.Get;

   function Keys (N : Map_Node) return String_Array
     renames Tinyaml.Nodes.Map.Keys;

   function Length (N : Map_Node) return Natural
     renames Tinyaml.Nodes.Map.Length;

   procedure Insert (N : in out Map_Node; Key : String; Value : Node_Access)
     renames Tinyaml.Nodes.Map.Insert;

   ---------------------------------------------------------------------------
   --  Re-export Navigation
   ---------------------------------------------------------------------------

   function Navigate
     (N    : YAML_Node'Class;
      Path : String) return Node_Access
     renames Tinyaml.Nodes.Navigation.Navigate;

   function Navigate (N : Node_Access; Path : String) return Node_Access
     renames Tinyaml.Nodes.Navigation.Navigate;

   function Get_String
     (N    : YAML_Node'Class;
      Path : String) return String
     renames Tinyaml.Nodes.Navigation.Get_String;

   function Get_String (N : Node_Access; Path : String) return String
     renames Tinyaml.Nodes.Navigation.Get_String;

end Tinyaml.Nodes.Prelude;
