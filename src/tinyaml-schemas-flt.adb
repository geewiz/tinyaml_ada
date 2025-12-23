with Tinyaml.Nodes;        use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar; use Tinyaml.Nodes.Scalar;

package body Tinyaml.Schemas.Flt is

   function Is_Float_Value (S : String) return Boolean is
      Dummy : Long_Float;
   begin
      Dummy := Long_Float'Value (S);
      pragma Unreferenced (Dummy);
      return True;
   exception
      when Constraint_Error => return False;
   end Is_Float_Value;

   overriding function Is_Valid
     (S : Float_Schema; N : Nodes.Node_Access) return Boolean
   is
      pragma Unreferenced (S);
   begin
      if N = null or else N.all not in Scalar_Node'Class then
         return False;
      end if;
      return Is_Float_Value (Scalar_Node (N.all).Value);
   end Is_Valid;

end Tinyaml.Schemas.Flt;
