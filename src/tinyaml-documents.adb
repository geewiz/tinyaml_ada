package body Tinyaml.Documents is

   use type Nodes.Node_Access;

   overriding procedure Finalize (Doc : in out Document) is
   begin
      if Doc.Root_Node /= null then
         Nodes.Free_Node (Doc.Root_Node);
      end if;
   end Finalize;

   function Is_Empty (Doc : Document) return Boolean is
   begin
      return Doc.Root_Node = null;
   end Is_Empty;

   function Root (Doc : Document) return Nodes.Node_Access is
   begin
      return Doc.Root_Node;
   end Root;

   procedure Set_Root (Doc : in out Document; N : Nodes.Node_Access) is
   begin
      --  If there was a previous root, free it first
      if Doc.Root_Node /= null then
         Nodes.Free_Node (Doc.Root_Node);
      end if;
      Doc.Root_Node := N;
   end Set_Root;

end Tinyaml.Documents;
