--  Map_Node: Ordered key-value pairs (preserves insertion order)

package Tinyaml.Nodes.Map is

   type Map_Node is new YAML_Node with private;

   --  Create a new empty map node
   function Create (Pos : Source_Span := No_Span) return Node_Access;

   --  Check if a key exists
   function Contains (N : Map_Node; Key : String) return Boolean;

   --  Get value for a key (raises Access_Error if not found)
   function Get (N : Map_Node; Key : String) return Node_Access;

   --  Get all keys in insertion order
   function Keys (N : Map_Node) return String_Array;

   --  Get number of entries
   function Length (N : Map_Node) return Natural;

   --  Insert a key-value pair (raises Parse_Error on duplicate key)
   procedure Insert
     (N     : in out Map_Node;
      Key   : String;
      Value : Node_Access);

   --  Node kind
   overriding function Kind (N : Map_Node) return Node_Kind is (Map_Kind);

private

   type Map_Node is new YAML_Node with record
      Entries   : String_Node_Maps.Map;
      Key_Order : String_Vectors.Vector;  --  Preserves insertion order
   end record;

end Tinyaml.Nodes.Map;
