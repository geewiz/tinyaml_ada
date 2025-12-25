--  TinyAML Parser - Builds document tree from YAML input
--

with Tinyaml.Documents;
with Tinyaml.Nodes;

package Tinyaml.Parser is

   --  Parse a YAML string and return the document root.
   --  Raises Parse_Error if the input is invalid.
   --
   --  NOTE: The returned Node_Access must be manually freed with Free_Node,
   --  or use Parse_Document instead for automatic memory management.
   function Parse (Input : String) return Nodes.Node_Access;

   --  Parse a YAML string and return a Document wrapper.
   --  The Document automatically frees the node tree when it goes out of scope.
   --  Raises Parse_Error if the input is invalid.
   function Parse_Document (Input : String) return Documents.Document;

end Tinyaml.Parser;
