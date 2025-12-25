package body Tinyaml.Nodes.Map is

   use Ada.Strings.Unbounded;

   function Contains (N : Map_Node; Key : String) return Boolean is
   begin
      return N.Entries.Contains (Key);
   end Contains;

   function Create (Pos : Source_Span := No_Span) return Node_Access is
   begin
      return new Map_Node'(Node_Span => Pos,
                           Entries   => String_Node_Maps.Empty_Map,
                           Key_Order => String_Vectors.Empty_Vector);
   end Create;

   function Get (N : Map_Node; Key : String) return Node_Access is
   begin
      if not N.Entries.Contains (Key) then
         raise Access_Error with "Key not found: " & Key;
      end if;
      return N.Entries.Element (Key);
   end Get;

   procedure Insert
     (N     : in out Map_Node;
      Key   : String;
      Value : Node_Access)
   is
   begin
      if N.Entries.Contains (Key) then
         raise Parse_Error with "Duplicate key: " & Key;
      end if;
      N.Entries.Insert (Key, Value);
      N.Key_Order.Append (Key);
   end Insert;

   function Keys (N : Map_Node) return String_Array is
      Result : String_Array (1 .. Natural (N.Key_Order.Length));
   begin
      for I in Result'Range loop
         Result (I) := To_Unbounded_String (N.Key_Order.Element (I));
      end loop;
      return Result;
   end Keys;

   function Length (N : Map_Node) return Natural is
   begin
      return Natural (N.Entries.Length);
   end Length;

end Tinyaml.Nodes.Map;
