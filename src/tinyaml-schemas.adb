with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tinyaml.Nodes;         use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar;  use Tinyaml.Nodes.Scalar;

package body Tinyaml.Schemas is

   function Validate_Node
     (S : Schema;
      N : Nodes.Node_Access) return Validation_Info
   is
   begin
      if Schema'Class (S).Is_Valid (N) then
         return Validation_Info'
           (Is_Valid => True,
            Message  => Null_Unbounded_String,
            Path     => Null_Unbounded_String,
            Value    => Null_Unbounded_String);
      else
         declare
            Val : Unbounded_String := Null_Unbounded_String;
         begin
            --  Extract value from scalar node if available
            if N /= null and then N.all in Scalar_Node'Class then
               Val := To_Unbounded_String (Scalar_Node (N.all).Value);
            end if;
            return Validation_Info'
              (Is_Valid => False,
               Message  => To_Unbounded_String
                 ("expected " & Schema'Class (S).Describe),
               Path     => Null_Unbounded_String,
               Value    => Val);
         end;
      end if;
   end Validate_Node;

end Tinyaml.Schemas;
