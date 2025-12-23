with Tinyaml.Nodes;        use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar; use Tinyaml.Nodes.Scalar;

package body Tinyaml.Schemas.Int is

   overriding function Describe (S : Int_Schema) return String is
   begin
      if S.Constraint.Min = Integer'First
        and then S.Constraint.Max = Integer'Last
      then
         return "integer";
      else
         return "integer (" & S.Constraint.Min'Image & " .."
           & S.Constraint.Max'Image & ")";
      end if;
   end Describe;

   overriding function Is_Valid
     (S : Int_Schema; N : Nodes.Node_Access) return Boolean
   is
   begin
      if N = null or else N.all not in Scalar_Node'Class then
         return False;
      end if;

      declare
         Val_Str : constant String := Scalar_Node (N.all).Value;
      begin
         return S.Constraint.Check (Val_Str);
      end;
   end Is_Valid;

end Tinyaml.Schemas.Int;
