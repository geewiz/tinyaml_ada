package body Tinyaml.Schemas.Constraints.Range_Constraint is

   function Is_Integer_Value (S : String) return Boolean is
      Dummy : Integer;
   begin
      Dummy := Integer'Value (S);
      pragma Unreferenced (Dummy);
      return True;
   exception
      when Constraint_Error => return False;
   end Is_Integer_Value;

   overriding function Check
     (C : Range_Constraint; Value : String) return Boolean
   is
   begin
      if not Is_Integer_Value (Value) then
         return False;
      end if;
      declare
         V : constant Integer := Integer'Value (Value);
      begin
         return V >= C.Min and then V <= C.Max;
      end;
   end Check;

   overriding function Describe (C : Range_Constraint) return String is
   begin
      return "range" & C.Min'Image & " .." & C.Max'Image;
   end Describe;

end Tinyaml.Schemas.Constraints.Range_Constraint;
