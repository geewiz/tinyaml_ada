--  Path Navigation: Navigate through nested node structures

package Tinyaml.Nodes.Navigation is

   --  Navigate through nested structures using dot-separated path
   --  Example: Navigate(Doc, "database.host") returns the host node
   --  Returns null if path not found
   function Navigate
     (N    : YAML_Node'Class;
      Path : String) return Node_Access;

   function Navigate (N : Node_Access; Path : String) return Node_Access;

   --  Convenience: Get scalar value at path
   function Get_String
     (N    : YAML_Node'Class;
      Path : String) return String;

   function Get_String (N : Node_Access; Path : String) return String;

end Tinyaml.Nodes.Navigation;
