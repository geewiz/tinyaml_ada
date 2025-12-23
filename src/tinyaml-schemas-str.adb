with Tinyaml.Nodes;        use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar; use Tinyaml.Nodes.Scalar;

package body Tinyaml.Schemas.Str is

   overriding function Is_Valid
     (S : Str_Schema; N : Nodes.Node_Access) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return N /= null and then N.all in Scalar_Node'Class;
   end Is_Valid;

end Tinyaml.Schemas.Str;
