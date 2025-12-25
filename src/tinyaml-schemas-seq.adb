with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Tinyaml.Nodes;            use Tinyaml.Nodes;
with Tinyaml.Nodes.Sequence;   use Tinyaml.Nodes.Sequence;

package body Tinyaml.Schemas.Seq is

   function To_Seq_Item (S : Schema'Class) return Seq_Item is
   begin
      return (Schema_Ref => new Schema'Class'(S));
   end To_Seq_Item;

   function Get_Schema (Item : Seq_Item) return Schema'Class is
   begin
      return Item.Schema_Ref.all;
   end Get_Schema;

   procedure Free_Item_Schema (Item : in out Seq_Item) is
   begin
      if Item.Schema_Ref /= null then
         Free_Schema (Item.Schema_Ref);
      end if;
   end Free_Item_Schema;

   overriding function Is_Valid
     (S : Seq_Schema; N : Nodes.Node_Access) return Boolean
   is
   begin
      if N = null or else N.all not in Sequence_Node'Class then
         return False;
      end if;

      declare
         Seq_N       : Sequence_Node renames Sequence_Node (N.all);
         Item_Schema : constant Schema'Class := Get_Schema (S.Item);
      begin
         for I in 1 .. Seq_N.Length loop
            if not Item_Schema.Is_Valid (Seq_N.Element (I)) then
               return False;
            end if;
         end loop;
         return True;
      end;
   end Is_Valid;

   overriding function Describe (S : Seq_Schema) return String is
   begin
      return "sequence of " & Get_Schema (S.Item).Describe;
   end Describe;

   overriding function Validate_Node
     (S : Seq_Schema; N : Node_Access) return Validation_Info
   is
   begin
      if N = null or else N.all not in Sequence_Node'Class then
         return Validation_Info'
           (Is_Valid => False,
            Message  => To_Unbounded_String ("expected " & S.Describe),
            Path     => Null_Unbounded_String,
            Value    => Null_Unbounded_String);
      end if;

      declare
         Seq_N       : Sequence_Node renames Sequence_Node (N.all);
         Item_Schema : constant Schema'Class := Get_Schema (S.Item);
         Item_Info   : Validation_Info;
      begin
         for I in 1 .. Seq_N.Length loop
            Item_Info := Item_Schema.Validate_Node (Seq_N.Element (I));
            if not Item_Info.Is_Valid then
               return Validation_Info'
                 (Is_Valid => False,
                  Message  => Item_Info.Message,
                  Path     => To_Unbounded_String ("[" & I'Image & "]") &
                              Item_Info.Path,
                  Value    => Item_Info.Value);
            end if;
         end loop;
         return Validation_Info'
           (Is_Valid => True,
            Message  => Null_Unbounded_String,
            Path     => Null_Unbounded_String,
            Value    => Null_Unbounded_String);
      end;
   end Validate_Node;

end Tinyaml.Schemas.Seq;
