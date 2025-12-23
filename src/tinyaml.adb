with Ada.Strings.Fixed;

package body Tinyaml is

   use Ada.Strings.Unbounded;

   --------------------
   -- Get_Error_Info --
   --------------------

   function Get_Error_Info
     (E : Ada.Exceptions.Exception_Occurrence) return Error_Info
   is
      Msg : constant String := Ada.Exceptions.Exception_Message (E);
   begin
      --  For now, return a simple error info.
      --  TODO: Parse structured position info from exception message
      --  when we implement that in the lexer/parser.
      return Error_Info'
        (Message  => To_Unbounded_String (Msg),
         Position => No_Position,
         Context  => Null_Unbounded_String);
   end Get_Error_Info;

   -----------
   -- Image --
   -----------

   function Image (Pos : Source_Position) return String is
   begin
      return "line" & Pos.Line'Image & ", column" & Pos.Column'Image;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Span : Source_Span) return String is
   begin
      if Span.First.Line = Span.Last.Line then
         if Span.First.Column = Span.Last.Column then
            return Image (Span.First);
         else
            return "line" & Span.First.Line'Image &
                   ", columns" & Span.First.Column'Image &
                   "-" & Ada.Strings.Fixed.Trim (Span.Last.Column'Image,
                                                  Ada.Strings.Left);
         end if;
      else
         return Image (Span.First) & " to " & Image (Span.Last);
      end if;
   end Image;

end Tinyaml;
