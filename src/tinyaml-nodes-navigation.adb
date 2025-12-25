with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Scalar;

package body Tinyaml.Nodes.Navigation is

   use Ada.Strings.Unbounded;

   function Get_String
     (N    : YAML_Node'Class;
      Path : String) return String
   is
      Target : constant Node_Access := Navigate (N, Path);
   begin
      if Target = null then
         raise Access_Error with "Path not found: " & Path;
      end if;
      if Target.all not in Scalar.Scalar_Node'Class then
         raise Access_Error with "Value at path is not a scalar: " & Path;
      end if;
      return To_String (Scalar.Scalar_Node (Target.all).Value);
   end Get_String;

   function Get_String (N : Node_Access; Path : String) return String is
   begin
      if N = null then
         raise Access_Error with "Cannot navigate from null node";
      end if;
      return Get_String (N.all, Path);
   end Get_String;

   function Navigate
     (N    : YAML_Node'Class;
      Path : String) return Node_Access
   is
      use Ada.Strings.Fixed;

      Current : Node_Access;
      Start   : Positive := Path'First;
      Dot_Pos : Natural;
   begin
      --  Handle empty path
      if Path'Length = 0 then
         raise Access_Error with "Empty path";
      end if;

      --  Start from a map node
      if N not in Map.Map_Node'Class then
         return null;  --  Can't navigate from non-map
      end if;

      --  Create an access to N (we need to work with access types)
      Current := new YAML_Node'Class'(N);

      --  Parse path components separated by dots
      loop
         Dot_Pos := Index (Path (Start .. Path'Last), ".");

         declare
            Key : constant String :=
              (if Dot_Pos = 0 then
                  Path (Start .. Path'Last)
               else
                  Path (Start .. Dot_Pos - 1));
         begin
            --  Navigate to next component
            if Current.all not in Map.Map_Node'Class then
               return null;  --  Can't navigate through non-map
            end if;

            if not Map.Map_Node (Current.all).Contains (Key) then
               return null;  --  Key not found
            end if;

            Current := Map.Map_Node (Current.all).Get (Key);

            --  Check if we're done
            if Dot_Pos = 0 then
               return Current;
            end if;

            Start := Dot_Pos + 1;
         end;
      end loop;
   end Navigate;

   function Navigate (N : Node_Access; Path : String) return Node_Access is
   begin
      if N = null then
         return null;
      end if;
      return Navigate (N.all, Path);
   end Navigate;

end Tinyaml.Nodes.Navigation;
