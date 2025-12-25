--  Any_Schema: Accepts any value

with Tinyaml.Nodes;

package Tinyaml.Schemas.Any is

   type Any_Schema is new Schema with null record;

   overriding function Is_Valid
     (S : Any_Schema; N : Nodes.Node_Access) return Boolean is (True);

   overriding function Describe (S : Any_Schema) return String is ("any");

end Tinyaml.Schemas.Any;
