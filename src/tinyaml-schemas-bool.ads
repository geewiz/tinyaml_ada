--  Bool_Schema: Accepts boolean values (true/false, yes/no, on/off)

with Tinyaml.Nodes;

package Tinyaml.Schemas.Bool is

   type Bool_Schema is new Schema with null record;

   overriding function Is_Valid
     (S : Bool_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe
     (S : Bool_Schema) return String is ("boolean (true/false/yes/no/on/off)");

end Tinyaml.Schemas.Bool;
