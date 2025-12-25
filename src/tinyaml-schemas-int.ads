--  Int_Schema: Accepts integer values with optional range constraint

with Tinyaml.Nodes;
with Tinyaml.Schemas.Constraints.Range_Constraint;

package Tinyaml.Schemas.Int is

   --  Re-export Range_Constraint for convenience
   subtype Range_Constraint is
     Tinyaml.Schemas.Constraints.Range_Constraint.Range_Constraint;

   --  Int_Schema with optional range constraint
   type Int_Schema is new Schema with record
      Constraint : Range_Constraint;
   end record;

   overriding function Is_Valid
     (S : Int_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe (S : Int_Schema) return String;

end Tinyaml.Schemas.Int;
