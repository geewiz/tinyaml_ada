--  TinyAML Parser - Builds document tree from YAML input
--

with Tinyaml.Nodes;

package Tinyaml.Parser is

   --  Parse a YAML string and return the document root.
   --  Raises Parse_Error if the input is invalid.
   function Parse (Input : String) return Nodes.Node_Access;

end Tinyaml.Parser;
