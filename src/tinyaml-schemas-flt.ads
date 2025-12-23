--  Float_Schema: Accepts floating-point values

with Tinyaml.Nodes;

package Tinyaml.Schemas.Flt is

   type Float_Schema is new Schema with null record;

   overriding function Is_Valid
     (S : Float_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe (S : Float_Schema) return String is ("float");

end Tinyaml.Schemas.Flt;
