# TinyAML - Ada YAML Library

A strict YAML subset parser for Ada 2022, inspired by [StrictYAML](https://github.com/crdoconnor/strictyaml). Designed for configuration files with a hybrid validation approach: parse to untyped tree, optionally validate with schemas.

## Build Commands

All build commands must run in a distrobox container:

```bash
distrobox enter ubuntu -- alr build                                                          # Build library
distrobox enter ubuntu -- bash -c 'cd tests && alr build'                                    # Build tests
distrobox enter ubuntu -- bash -c 'cd examples && alr build'                                 # Build examples
distrobox enter ubuntu -- ./tests/bin/test_runner                                            # Run tests
```

## Project Structure

```
tinyaml_ada/
├── alire.toml                 # Alire package manifest (crate: tinyaml)
├── tinyaml.gpr                # Main GPR project file
├── src/
│   ├── tinyaml.ads/adb        # Root: exceptions, Source_Position, Source_Span
│   ├── tinyaml-lexer.ads/adb  # Tokenizer with indentation tracking
│   ├── tinyaml-nodes.ads/adb          # Abstract YAML_Node, Node_Access, type checking
│   ├── tinyaml-nodes-scalar.ads/adb   # Scalar_Node
│   ├── tinyaml-nodes-sequence.ads/adb # Sequence_Node
│   ├── tinyaml-nodes-map.ads/adb      # Map_Node
│   ├── tinyaml-nodes-navigation.ads/adb # Navigate, Get_String
│   ├── tinyaml-nodes-prelude.ads      # Convenience re-exports
│   ├── tinyaml-parser.ads/adb         # Recursive descent parser
│   ├── tinyaml-schemas.ads/adb        # Abstract Schema base
│   ├── tinyaml-schemas-*.ads/adb      # Concrete schema types
│   ├── tinyaml-schemas-prelude.ads    # Convenience re-exports
│   └── tinyaml-validation.ads/adb     # Validation engine
├── tests/
│   ├── tinyaml_tests.gpr
│   ├── test_harness.ads/adb   # Simple test framework
│   ├── test_lexer.ads/adb
│   ├── test_parser.ads/adb
│   ├── test_validation.ads/adb
│   └── test_runner.adb
└── examples/
    ├── tinyaml_examples.gpr
    ├── basic_parsing.adb      # Parse without validation
    ├── with_validation.adb    # Schema validation
    └── error_handling.adb     # Error handling patterns
```

## Design Decisions

- **Ada 2022** with FSF GNAT compatible subset (no user-defined literals, parallel blocks)
- **StrictYAML philosophy**: No implicit typing, no flow style, no anchors/aliases, no tags
- **Hybrid validation**: Parse to untyped tree, optional schema validation layer
- **OOP design**: Tagged type hierarchies for nodes and schemas
- **Strings only**: No file I/O - application provides content as String

## API Style

Uses idiomatic Ada with named access types and function call notation.
Prelude packages provide convenient imports:

```ada
with Tinyaml.Parser;
with Tinyaml.Nodes;           use Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude;   use Tinyaml.Nodes.Prelude;
with Tinyaml.Schemas.Prelude; use Tinyaml.Schemas.Prelude;

Doc := Tinyaml.Parser.Parse (Config);

--  Navigation with Node_Access
Put_Line (Get_String (Doc, "database.host"));
Features := Navigate (Doc, "features");

--  Type checking requires .all
if Is_Sequence (Features.all) then ...

--  Get scalar value from Node_Access
Put_Line (Value (Seq.Element (I)));
```

## YAML Features

**Supported:**
- Block scalars (literal `|`, folded `>`)
- Block sequences (`-`)
- Block mappings (`:`)
- Comments (`#`)
- Quoted strings (single and double)
- Escape sequences in double-quoted strings

**Rejected (raises Parse_Error):**
- Flow style (`{...}`, `[...]`)
- Anchors and aliases (`&`, `*`)
- Tags (`!`)
- Duplicate keys

## Known Issues

- Style warnings for alphabetical ordering - intentionally deferred
- Some internal helper functions lack specs - intentionally local

## Schema API (Redesigned)

The schema API uses class-wide types with dot notation:

1. **Direct instantiation with aggregates** - no factory functions where possible
2. **Class-wide types** for dot-notation enabled interface
3. **Hidden access types** - users work with tagged types directly
4. **Procedural field addition** instead of functional chaining
5. **Optional as a field modifier** instead of a wrapper type
6. **Abstract Constraint hierarchy** for extensible validation rules
7. **Seq_Item holder** for polymorphic sequence item storage

### Type Hierarchies

```
Schema (abstract)              Constraint (abstract)
├── Str_Schema                 └── Range_Constraint
├── Int_Schema                     (future: Length_Constraint,
├── Float_Schema                    Pattern_Constraint, etc.)
├── Bool_Schema
├── Enum_Schema
├── Seq_Schema
├── Map_Schema
└── Any_Schema
```

### API

```ada
package Tinyaml.Schemas is

   --  Schema hierarchy (no access types exposed to users)
   type Schema is abstract tagged private;

   function Is_Valid (S : Schema; N : Nodes.YAML_Node'Class) return Boolean is abstract;
   function Describe (S : Schema) return String is abstract;

   --  Constraint hierarchy
   type Constraint is abstract tagged private;

   function Check (C : Constraint; Value : String) return Boolean is abstract;
   function Describe (C : Constraint) return String is abstract;

   --  Range_Constraint (first child of Constraint)
   type Range_Constraint is new Constraint with private;

   ---------------------------------------------------------------------------
   --  Seq_Item Holder (for polymorphic sequence items)
   ---------------------------------------------------------------------------

   type Seq_Item is tagged private;

   function Seq_Item (S : Schema'Class) return Seq_Item;

   ---------------------------------------------------------------------------
   --  Concrete Schema Types (instantiate directly with aggregates)
   ---------------------------------------------------------------------------

   type Str_Schema is new Schema with private;
   type Float_Schema is new Schema with private;
   type Bool_Schema is new Schema with private;
   type Any_Schema is new Schema with private;

   type Int_Schema is new Schema with private;
   --  Use aggregate: (Constraint => (Min => 1, Max => 100))

   type Enum_Schema is new Schema with private;
   --  Use aggregate: (Values => String_Vectors.To_Vector(...))

   type Seq_Schema is new Schema with private;
   --  Use aggregate: (Item => Seq_Item (Str_Schema'(...)))

   type Map_Schema is new Schema with private;
   --  Instantiate directly, then add fields with dot notation

   ---------------------------------------------------------------------------
   --  Field Addition (class-wide parameters enable dot notation)
   ---------------------------------------------------------------------------

   procedure Str (M : in out Map_Schema'Class; Name : String;
                  Optional : Boolean := False);
   procedure Int (M : in out Map_Schema'Class; Name : String;
                  Optional : Boolean := False);
   procedure Int (M : in out Map_Schema'Class; Name : String;
                  Constraint : Range_Constraint;
                  Optional : Boolean := False);
   procedure Flt (M : in out Map_Schema'Class; Name : String;
                  Optional : Boolean := False);
   procedure Bool (M : in out Map_Schema'Class; Name : String;
                   Optional : Boolean := False;
                   Default : String := "");
   procedure Enum (M : in out Map_Schema'Class; Name : String;
                   Values : String_Array;
                   Optional : Boolean := False);
   procedure Seq (M : in out Map_Schema'Class; Name : String;
                  Item : Seq_Item;
                  Optional : Boolean := False);
   procedure Any (M : in out Map_Schema'Class; Name : String;
                  Optional : Boolean := False);

   --  Nested map (returns the nested map for further configuration)
   function Map (M : in out Map_Schema'Class; Name : String;
                 Optional : Boolean := False) return Map_Schema;

private

   type Schema is abstract tagged null record;

   type Constraint is abstract tagged null record;

   type Range_Constraint is new Constraint with record
      Min : Integer := Integer'First;
      Max : Integer := Integer'Last;
   end record;

   --  Access types hidden from users, used internally for storage
   type Schema_Access is access all Schema'Class;

   --  Seq_Item wraps access type for clean aggregate syntax
   type Seq_Item is tagged record
      Schema : Schema_Access;
   end record;

   type Str_Schema is new Schema with null record;
   type Float_Schema is new Schema with null record;
   type Bool_Schema is new Schema with null record;
   type Any_Schema is new Schema with null record;

   type Int_Schema is new Schema with record
      Constraint : Range_Constraint := (others => <>);
   end record;

   type Enum_Schema is new Schema with record
      Values : String_Vectors.Vector;
   end record;

   type Seq_Schema is new Schema with record
      Item : Seq_Item;
   end record;

   type Field_Info is record
      Field_Schema : Schema_Access;
      Is_Required  : Boolean := True;
      Default_Val  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Map_Schema is new Schema with record
      Fields : Field_Maps.Map;
   end record;

end Tinyaml.Schemas;
```

### Usage Example

```ada
with Tinyaml.Schemas; use Tinyaml.Schemas;

procedure Define_Config_Schema is
   --  Direct instantiation, no factory functions
   M : Map_Schema;
begin
   --  Simple fields with dot notation
   M.Str ("host");
   M.Str ("name");
   M.Int ("port", Constraint => (Min => 1, Max => 65535));
   M.Bool ("debug", Optional => True, Default => "false");

   --  Nested map
   declare
      DB : Map_Schema := M.Map ("database");
   begin
      DB.Str ("driver");
      DB.Str ("connection");
   end;

   --  Sequence of strings
   M.Seq ("features", Item => Seq_Item (Str_Schema'(others => <>)),
          Optional => True);

   --  Sequence of maps (e.g., Docker Compose volumes)
   declare
      Volume_Schema : Map_Schema;
   begin
      Volume_Schema.Str ("type");
      Volume_Schema.Str ("source");
      Volume_Schema.Str ("target");
      M.Seq ("volumes", Item => Seq_Item (Volume_Schema));
   end;
end Define_Config_Schema;
```

### Key Design Features

- **Direct instantiation** - `M : Map_Schema;`
- **Dot notation** - `M.Str ("host")` for building schemas
- **Aggregates for construction** - `(Constraint => (Min => 1, Max => 100))`
- **Seq_Item holder** - `To_Seq_Item (Map_Schema'(...))` for polymorphic sequences
- **Class-wide interface** - users work with tagged types, not access types
- **Hidden access types** - only in private part, for internal storage
- **Optional as parameter** - `Optional => True` instead of wrapper type
- **Abstract `Constraint` hierarchy** - extensible (start with `Range_Constraint`)
