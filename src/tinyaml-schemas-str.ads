--  Str_Schema: Accepts any string value

with Tinyaml.Nodes;

package Tinyaml.Schemas.Str is

   type Str_Schema is new Schema with null record;

   overriding function Is_Valid
     (S : Str_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe (S : Str_Schema) return String is ("string");

end Tinyaml.Schemas.Str;
