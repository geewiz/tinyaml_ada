--  TinyAML Document - RAII wrapper for parsed YAML documents
--
--  Document owns the root node and automatically deallocates the entire
--  document tree when it goes out of scope.

with Ada.Finalization;

with Tinyaml.Nodes;

package Tinyaml.Documents is

   ---------------------------------------------------------------------------
   --  Document Type (RAII wrapper)
   ---------------------------------------------------------------------------

   type Document is new Ada.Finalization.Limited_Controlled with private;

   --  Check if document has a valid root node
   function Is_Empty (Doc : Document) return Boolean;

   --  Get the root node for navigation (null if empty)
   function Root (Doc : Document) return Nodes.Node_Access;

   ---------------------------------------------------------------------------
   --  Internal: Used by Parser to construct Document
   ---------------------------------------------------------------------------

   procedure Set_Root (Doc : in out Document; N : Nodes.Node_Access);

private

   type Document is new Ada.Finalization.Limited_Controlled with record
      Root_Node : Nodes.Node_Access := null;
   end record;

   overriding procedure Finalize (Doc : in out Document);
   --  Recursively frees all nodes in the document tree

end Tinyaml.Documents;
