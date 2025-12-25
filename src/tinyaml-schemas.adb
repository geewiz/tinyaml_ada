with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Tinyaml.Nodes;         use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar;  use Tinyaml.Nodes.Scalar;
with Tinyaml.Schemas.Map;
with Tinyaml.Schemas.Seq;

package body Tinyaml.Schemas is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => Schema'Class,
      Name   => Schema_Access);

   procedure Free_Schema (S : in out Schema_Access) is
   begin
      if S = null then
         return;
      end if;

      --  Recursively free nested schemas based on schema type
      if S.all in Map.Map_Schema'Class then
         --  Map_Schema has fields that contain Schema_Access
         declare
            M : Map.Map_Schema renames Map.Map_Schema (S.all);
         begin
            Map.Free_Fields (M);
         end;
      elsif S.all in Seq.Seq_Schema'Class then
         --  Seq_Schema has a Seq_Item containing Schema_Access
         declare
            Sq : Seq.Seq_Schema renames Seq.Seq_Schema (S.all);
         begin
            Seq.Free_Item_Schema (Sq.Item);
         end;
      end if;

      --  Deallocate this schema
      Deallocate (S);
   end Free_Schema;

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
