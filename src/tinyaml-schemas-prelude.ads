--  Convenience package that re-exports all schema types
--
--  Usage:
--    with Tinyaml.Schemas.Prelude; use Tinyaml.Schemas.Prelude;
--
--    Schema : Map_Schema;
--    Schema.Str ("host");
--    Schema.Int ("port", Constraint => (Min => 1, Max => 65535));

with Tinyaml.Schemas.Constraints.Range_Constraint;
with Tinyaml.Schemas.Str;
with Tinyaml.Schemas.Int;
with Tinyaml.Schemas.Flt;
with Tinyaml.Schemas.Bool;
with Tinyaml.Schemas.Enum;
with Tinyaml.Schemas.Seq;
with Tinyaml.Schemas.Map;
with Tinyaml.Schemas.Any;

package Tinyaml.Schemas.Prelude is

   --  Re-export Range_Constraint
   subtype Range_Constraint is
     Tinyaml.Schemas.Constraints.Range_Constraint.Range_Constraint;

   --  Re-export Str_Schema
   subtype Str_Schema is Tinyaml.Schemas.Str.Str_Schema;

   --  Re-export Int_Schema
   subtype Int_Schema is Tinyaml.Schemas.Int.Int_Schema;

   --  Re-export Float_Schema
   subtype Float_Schema is Tinyaml.Schemas.Flt.Float_Schema;

   --  Re-export Bool_Schema
   subtype Bool_Schema is Tinyaml.Schemas.Bool.Bool_Schema;

   --  Re-export Enum_Schema and String_Array
   subtype Enum_Schema is Tinyaml.Schemas.Enum.Enum_Schema;
   subtype String_Array is Tinyaml.Schemas.Enum.String_Array;
   function Make_Enum (Values : String_Array) return Enum_Schema
     renames Tinyaml.Schemas.Enum.Make_Enum;

   --  Re-export Seq_Schema and Seq_Item
   subtype Seq_Schema is Tinyaml.Schemas.Seq.Seq_Schema;
   subtype Seq_Item is Tinyaml.Schemas.Seq.Seq_Item;
   function To_Seq_Item (S : Schema'Class) return Seq_Item
     renames Tinyaml.Schemas.Seq.To_Seq_Item;
   function Get_Schema (Item : Seq_Item) return Schema'Class
     renames Tinyaml.Schemas.Seq.Get_Schema;

   --  Re-export Map_Schema
   subtype Map_Schema is Tinyaml.Schemas.Map.Map_Schema;

   --  Re-export Any_Schema
   subtype Any_Schema is Tinyaml.Schemas.Any.Any_Schema;

end Tinyaml.Schemas.Prelude;
