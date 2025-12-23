--  Map_Schema: Accepts a mapping with specific field schemas

private with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with Tinyaml.Nodes;
with Tinyaml.Schemas.Constraints.Range_Constraint;
use Tinyaml.Schemas.Constraints.Range_Constraint;
with Tinyaml.Schemas.Seq;
with Tinyaml.Schemas.Enum;

package Tinyaml.Schemas.Map is

   type Map_Schema is new Schema with private;

   overriding function Is_Valid
     (S : Map_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe (S : Map_Schema) return String is ("mapping");

   overriding function Validate_Node
     (S : Map_Schema; N : Nodes.Node_Access) return Validation_Info;

   ---------------------------------------------------------------------------
   --  Field Addition Procedures (dot notation enabled via class-wide params)
   ---------------------------------------------------------------------------

   procedure Str
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False);

   procedure Int
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False);

   procedure Int
     (M          : in out Map_Schema'Class;
      Name       : String;
      Constraint : Range_Constraint;
      Optional   : Boolean := False);

   procedure Flt
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False);

   procedure Bool
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False;
      Default  : String := "");

   procedure Enum
     (M        : in out Map_Schema'Class;
      Name     : String;
      Values   : Tinyaml.Schemas.Enum.String_Array;
      Optional : Boolean := False);

   procedure Seq
     (M        : in out Map_Schema'Class;
      Name     : String;
      Item     : Tinyaml.Schemas.Seq.Seq_Item;
      Optional : Boolean := False);

   procedure Any
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False);

   --  Nested map: adds a pre-built Map_Schema as a field
   procedure Map
     (M        : in out Map_Schema'Class;
      Name     : String;
      Schema   : Map_Schema;
      Optional : Boolean := False);

private

   type Field_Info is record
      Field_Schema : Schema_Access;
      Is_Required  : Boolean := True;
      Default_Val  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Field_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Field_Info);

   type Map_Schema is new Schema with record
      Fields : Field_Maps.Map;
   end record;

end Tinyaml.Schemas.Map;
