with Ada.Characters.Handling;
with Tinyaml.Nodes;        use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar; use Tinyaml.Nodes.Scalar;

package body Tinyaml.Schemas.Bool is

   function Is_Boolean_Value (S : String) return Boolean is
      Lower : constant String := Ada.Characters.Handling.To_Lower (S);
   begin
      return Lower = "true" or else Lower = "false"
        or else Lower = "yes" or else Lower = "no"
        or else Lower = "on" or else Lower = "off";
   end Is_Boolean_Value;

   overriding function Is_Valid
     (S : Bool_Schema; N : Nodes.Node_Access) return Boolean
   is
      pragma Unreferenced (S);
   begin
      if N = null or else N.all not in Scalar_Node'Class then
         return False;
      end if;
      return Is_Boolean_Value (Scalar_Node (N.all).Value);
   end Is_Valid;

end Tinyaml.Schemas.Bool;
