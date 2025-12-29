with Ada.Exceptions;
with Ada.Strings.Unbounded;

package Tinyaml is

   pragma Preelaborate;

   --  Library version
   Version : constant String := "0.2.0";

   --  Exceptions
   --
   --  Parse_Error: Raised when the input cannot be tokenized or parsed.
   --  Contains source position and context in the exception message.
   Parse_Error : exception;

   --  Validation_Error: Raised when a document doesn't match its schema.
   --  Contains details about which field failed and why.
   Validation_Error : exception;

   --  Access_Error: Raised when accessing a node incorrectly.
   --  Examples: calling Get on a scalar, accessing missing key.
   Access_Error : exception;

   --  Source position tracking for error messages
   type Source_Position is record
      Line   : Positive := 1;
      Column : Positive := 1;
      Offset : Natural  := 0;  --  Byte offset from start of input
   end record;

   --  A span of source text (from first to last position, inclusive)
   type Source_Span is record
      First : Source_Position;
      Last  : Source_Position;
   end record;

   --  No_Position/No_Span: Unknown or not-applicable source location
   No_Position : constant Source_Position :=
     (Line => 1, Column => 1, Offset => 0);
   No_Span : constant Source_Span :=
     (First => (Line => 1, Column => 1, Offset => 0),
      Last  => (Line => 1, Column => 1, Offset => 0));

   --  Structured error information extracted from exceptions
   type Error_Info is record
      Message  : Ada.Strings.Unbounded.Unbounded_String;
      Position : Source_Position;
      Context  : Ada.Strings.Unbounded.Unbounded_String;  --  Source snippet
   end record;

   --  Extract structured error info from an exception occurrence.
   --  Works with Parse_Error, Validation_Error, and Access_Error.
   --  For other exceptions, returns the exception message with No_Position.
   function Get_Error_Info
     (E : Ada.Exceptions.Exception_Occurrence) return Error_Info;

   --  Format a source position for display (e.g., "line 5, column 12")
   function Image (Pos : Source_Position) return String;

   --  Format a source span for display
   function Image (Span : Source_Span) return String;

end Tinyaml;
