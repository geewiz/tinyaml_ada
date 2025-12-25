--  Range_Constraint: Validates integer values within a range

package Tinyaml.Schemas.Constraints.Range_Constraint is

   type Range_Constraint is new Constraint with record
      Min : Integer := Integer'First;
      Max : Integer := Integer'Last;
   end record;

   overriding function Check
     (C : Range_Constraint; Value : String) return Boolean;

   overriding function Describe (C : Range_Constraint) return String;

end Tinyaml.Schemas.Constraints.Range_Constraint;
