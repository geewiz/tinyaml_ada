--  Abstract Constraint Base Type for schema validation

package Tinyaml.Schemas.Constraints is

   type Constraint is abstract tagged null record;

   function Check (C : Constraint; Value : String) return Boolean is abstract;
   function Describe (C : Constraint) return String is abstract;

end Tinyaml.Schemas.Constraints;
