--  Seq_Schema: Accepts a sequence where all items match Item schema

with Tinyaml.Nodes;

package Tinyaml.Schemas.Seq is

   --  Seq_Item holds a polymorphic schema reference
   type Seq_Item is tagged private;

   function To_Seq_Item (S : Schema'Class) return Seq_Item;
   function Get_Schema (Item : Seq_Item) return Schema'Class;

   --  Seq_Schema validates sequences with items matching a schema
   type Seq_Schema is new Schema with record
      Item : Seq_Item;
   end record;

   overriding function Is_Valid
     (S : Seq_Schema; N : Nodes.Node_Access) return Boolean;

   overriding function Describe (S : Seq_Schema) return String;

   overriding function Validate_Node
     (S : Seq_Schema; N : Nodes.Node_Access) return Validation_Info;

private

   type Seq_Item is tagged record
      Schema_Ref : Schema_Access;
   end record;

end Tinyaml.Schemas.Seq;
