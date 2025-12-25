with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tinyaml.Nodes;         use Tinyaml.Nodes;
with Tinyaml.Nodes.Map;     use Tinyaml.Nodes.Map;
with Tinyaml.Schemas.Str;
with Tinyaml.Schemas.Int;
with Tinyaml.Schemas.Flt;
with Tinyaml.Schemas.Bool;
with Tinyaml.Schemas.Any;

package body Tinyaml.Schemas.Map is

   procedure Free_Fields (M : in out Map_Schema) is
      use Field_Maps;
   begin
      for C in M.Fields.Iterate loop
         declare
            Info  : Field_Info := Element (C);
            S_Acc : Schema_Access := Info.Field_Schema;
         begin
            if S_Acc /= null then
               Free_Schema (S_Acc);
            end if;
         end;
      end loop;
      M.Fields.Clear;
   end Free_Fields;

   procedure Add_Field
     (M            : in out Map_Schema'Class;
      Name         : String;
      Field_Schema : Schema_Access;
      Required     : Boolean;
      Default      : String := "");

   procedure Add_Field
     (M            : in out Map_Schema'Class;
      Name         : String;
      Field_Schema : Schema_Access;
      Required     : Boolean;
      Default      : String := "")
   is
   begin
      M.Fields.Insert
        (Name,
         Field_Info'(Field_Schema => Field_Schema,
                     Is_Required  => Required,
                     Default_Val  => To_Unbounded_String (Default)));
   end Add_Field;

   procedure Any
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False)
   is
   begin
      Add_Field (M, Name, new Tinyaml.Schemas.Any.Any_Schema,
                 Required => not Optional);
   end Any;

   procedure Bool
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False;
      Default  : String := "")
   is
   begin
      Add_Field (M, Name, new Tinyaml.Schemas.Bool.Bool_Schema,
                 Required => not Optional, Default => Default);
   end Bool;

   procedure Enum
     (M        : in out Map_Schema'Class;
      Name     : String;
      Values   : Tinyaml.Schemas.Enum.String_Array;
      Optional : Boolean := False)
   is
      use Tinyaml.Schemas.Enum;
   begin
      Add_Field (M, Name, new Enum_Schema'(Make_Enum (Values)),
                 Required => not Optional);
   end Enum;

   procedure Flt
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False)
   is
   begin
      Add_Field (M, Name, new Tinyaml.Schemas.Flt.Float_Schema,
                 Required => not Optional);
   end Flt;

   procedure Int
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False)
   is
   begin
      Add_Field (M, Name, new Tinyaml.Schemas.Int.Int_Schema,
                 Required => not Optional);
   end Int;

   procedure Int
     (M          : in out Map_Schema'Class;
      Name       : String;
      Constraint : Tinyaml.Schemas.Int.Range_Constraint;
      Optional   : Boolean := False)
   is
      use Tinyaml.Schemas.Int;
   begin
      Add_Field (M, Name,
                 new Int_Schema'(Constraint => Constraint),
                 Required => not Optional);
   end Int;

   overriding function Is_Valid
     (S : Map_Schema; N : Node_Access) return Boolean
   is
      use Field_Maps;
   begin
      if N = null or else N.all not in Map_Node'Class then
         return False;
      end if;

      declare
         Map_N : Map_Node renames Map_Node (N.all);
      begin
         for C in S.Fields.Iterate loop
            declare
               Name : constant String := Key (C);
               Info : constant Field_Info := Element (C);
            begin
               if Map_N.Contains (Name) then
                  if not Info.Field_Schema.Is_Valid (Map_N.Get (Name)) then
                     return False;
                  end if;
               elsif Info.Is_Required then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end;
   end Is_Valid;

   procedure Map
     (M        : in out Map_Schema'Class;
      Name     : String;
      Schema   : Map_Schema;
      Optional : Boolean := False)
   is
   begin
      Add_Field (M, Name, new Map_Schema'(Schema), Required => not Optional);
   end Map;

   procedure Seq
     (M        : in out Map_Schema'Class;
      Name     : String;
      Item     : Tinyaml.Schemas.Seq.Seq_Item;
      Optional : Boolean := False)
   is
      use Tinyaml.Schemas.Seq;
   begin
      Add_Field (M, Name, new Seq_Schema'(Item => Item),
                 Required => not Optional);
   end Seq;

   procedure Str
     (M        : in out Map_Schema'Class;
      Name     : String;
      Optional : Boolean := False)
   is
   begin
      Add_Field (M, Name, new Tinyaml.Schemas.Str.Str_Schema,
                 Required => not Optional);
   end Str;

   overriding function Validate_Node
     (S : Map_Schema; N : Node_Access) return Validation_Info
   is
      use Field_Maps;
   begin
      if N = null or else N.all not in Map_Node'Class then
         return Validation_Info'
           (Is_Valid => False,
            Message  => To_Unbounded_String ("expected mapping"),
            Path     => Null_Unbounded_String,
            Value    => Null_Unbounded_String);
      end if;

      declare
         Map_N : Map_Node renames Map_Node (N.all);
      begin
         for C in S.Fields.Iterate loop
            declare
               Name : constant String := Key (C);
               Info : constant Field_Info := Element (C);
            begin
               if Map_N.Contains (Name) then
                  declare
                     Field_Result : constant Validation_Info :=
                       Info.Field_Schema.Validate_Node (Map_N.Get (Name));
                  begin
                     if not Field_Result.Is_Valid then
                        return Validation_Info'
                          (Is_Valid => False,
                           Message  => Field_Result.Message,
                           Path     => To_Unbounded_String (Name) &
                                       (if Length (Field_Result.Path) > 0
                                        then To_Unbounded_String (".") &
                                             Field_Result.Path
                                        else Null_Unbounded_String),
                           Value    => Field_Result.Value);
                     end if;
                  end;
               elsif Info.Is_Required then
                  return Validation_Info'
                    (Is_Valid => False,
                     Message  => To_Unbounded_String
                       ("missing required field '" & Name & "'"),
                     Path     => Null_Unbounded_String,
                     Value    => Null_Unbounded_String);
               end if;
            end;
         end loop;

         return Validation_Info'
           (Is_Valid => True,
            Message  => Null_Unbounded_String,
            Path     => Null_Unbounded_String,
            Value    => Null_Unbounded_String);
      end;
   end Validate_Node;

end Tinyaml.Schemas.Map;
