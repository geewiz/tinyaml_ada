package body Tinyaml.Nodes.Scalar is

   use Ada.Strings.Unbounded;

   function Create
     (Value : String;
      Pos   : Source_Span := No_Span) return Node_Access
   is
   begin
      return new Scalar_Node'(Node_Span => Pos,
                              Str_Value => To_Unbounded_String (Value));
   end Create;

   function Value (N : Scalar_Node) return String is
   begin
      return To_String (N.Str_Value);
   end Value;

   function Value (N : Scalar_Node) return Unbounded_String is
   begin
      return N.Str_Value;
   end Value;

   function Value (N : Node_Access) return String is
   begin
      if N = null then
         raise Access_Error with "Cannot get value of null node";
      end if;
      if N.all not in Scalar_Node'Class then
         raise Access_Error with "Node is not a scalar";
      end if;
      return To_String (Scalar_Node (N.all).Str_Value);
   end Value;

end Tinyaml.Nodes.Scalar;
