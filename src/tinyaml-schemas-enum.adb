with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tinyaml.Nodes;         use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar;  use Tinyaml.Nodes.Scalar;

package body Tinyaml.Schemas.Enum is

   function Make_Enum (Values : String_Array) return Enum_Schema is
      Result : Enum_Schema;
   begin
      for V of Values loop
         Result.Allowed_Values.Append (V.all);
      end loop;
      return Result;
   end Make_Enum;

   overriding function Is_Valid
     (S : Enum_Schema; N : Nodes.Node_Access) return Boolean
   is
   begin
      if N = null or else N.all not in Scalar_Node'Class then
         return False;
      end if;

      declare
         Val : constant String := Scalar_Node (N.all).Value;
      begin
         for Allowed of S.Allowed_Values loop
            if Allowed = Val then
               return True;
            end if;
         end loop;
         return False;
      end;
   end Is_Valid;

   overriding function Describe (S : Enum_Schema) return String is
      Result : Unbounded_String := To_Unbounded_String ("one of: ");
      First  : Boolean := True;
   begin
      for V of S.Allowed_Values loop
         if not First then
            Append (Result, ", ");
         end if;
         Append (Result, V);
         First := False;
      end loop;
      return To_String (Result);
   end Describe;

end Tinyaml.Schemas.Enum;
