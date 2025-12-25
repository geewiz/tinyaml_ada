--  Enum_Schema: Accepts one of a set of allowed values

private with Ada.Containers.Indefinite_Vectors;

with Tinyaml.Nodes;

package Tinyaml.Schemas.Enum is

   type String_Array is array (Positive range <>) of access constant String;

   type Enum_Schema is new Schema with private;

   function Make_Enum (Values : String_Array) return Enum_Schema;

   overriding function Is_Valid
     (S : Enum_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe (S : Enum_Schema) return String;

private

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   type Enum_Schema is new Schema with record
      Allowed_Values : String_Vectors.Vector;
   end record;

end Tinyaml.Schemas.Enum;
