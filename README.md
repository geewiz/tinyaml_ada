# TinyAML

A strict YAML subset parser for Ada 2022, inspired by [StrictYAML](https://github.com/crdoconnor/strictyaml).

TinyAML parses a safe subset of YAML suitable for configuration files. All values are strings until explicitly validated against a schema.

> [!NOTE]
> This library is under early development. Until release of version 1.0, expect breaking changes even with only minor version bumps. Only from 1.0 on, we'll adhere fully to semantic versioning.

## Installation

Add to your Alire project:

```bash
alr with tinyaml
```

## Quick Start

```ada
with Ada.Text_IO;
with Tinyaml.Documents;     use Tinyaml.Documents;
with Tinyaml.Parser;
with Tinyaml.Nodes;         use Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude; use Tinyaml.Nodes.Prelude;

procedure Example is
   Config : constant String :=
     "database:" & ASCII.LF &
     "  host: localhost" & ASCII.LF &
     "  port: 5432";

   --  Document automatically frees memory when it goes out of scope
   Doc : constant Document := Tinyaml.Parser.Parse_Document (Config);
begin
   Ada.Text_IO.Put_Line (Get_String (Root (Doc), "database.host"));  -- "localhost"
end Example;
```

## Schema Validation

```ada
with Ada.Text_IO;             use Ada.Text_IO;
with Tinyaml.Nodes;           use Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude;   use Tinyaml.Nodes.Prelude;
with Tinyaml.Schemas.Prelude; use Tinyaml.Schemas.Prelude;
with Tinyaml.Validation;      use Tinyaml.Validation;

procedure Validate_Config (Input : String) is
   Server_Schema : Map_Schema;
   Result        : Validation_Result;
begin
   Server_Schema.Str ("host");
   Server_Schema.Int ("port", Constraint => (Min => 1, Max => 65535));

   Result := Parse_And_Validate (Input, Server_Schema);

   if Result.Is_Valid then
      Put_Line (Get_String (Result.Root, "host"));
   else
      Put_Line (Result.Error_Message);
   end if;
end Validate_Config;
```

## Supported YAML

```yaml
# Comments
name: Alice                    # Scalars
port: 8080                     # (all values are strings)

database:                      # Nested mappings
  host: localhost

features:                      # Sequences
  - logging
  - metrics

description: |                 # Block scalars
  Multi-line
  text here
```

## Not Supported (by design)

These YAML features are intentionally rejected to keep configurations simple and predictable:

- Flow style: `{key: value}`, `[a, b, c]`
- Anchors and aliases: `&anchor`, `*alias`
- Tags: `!tag`
- Implicit typing (everything is a string until schema-validated)
- Multiple documents

## API Reference

### Parsing

```ada
--  Recommended: automatic memory management with Document wrapper
Doc : Document := Tinyaml.Parser.Parse_Document (Input);
Root_Node : Node_Access := Root (Doc);  --  Access the root node
--  Memory is freed when Doc goes out of scope

--  Alternative: manual memory management
Doc : Node_Access := Tinyaml.Parser.Parse (Input);
--  ... use Doc ...
Free_Node (Doc);  --  Must call explicitly to avoid memory leak
```

### Node Types and Navigation

Use `Tinyaml.Nodes` for base types and `Tinyaml.Nodes.Prelude` for child package types:

```ada
with Tinyaml.Nodes;         use Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude; use Tinyaml.Nodes.Prelude;

--  Type checking
Is_Scalar (Node.all)
Is_Sequence (Node.all)
Is_Map (Node.all)

--  Scalar operations
Scalar_Node (Node.all).Value    -- Get scalar string
Value (Node)                    -- Get scalar value from Node_Access

--  Sequence operations
Sequence_Node (Node.all).Length     -- Sequence length
Sequence_Node (Node.all).Element (I)  -- Get item (1-based)

--  Map operations
Map_Node (Node.all).Contains (Key)  -- Check key exists
Map_Node (Node.all).Get (Key)       -- Get value by key

--  Path navigation
Navigate (Doc, "path.to.value")     -- Returns Node_Access or null
Get_String (Doc, "path.to.value")   -- Returns String, raises on missing
```

Individual packages (`Tinyaml.Nodes.Scalar`, `.Sequence`, `.Map`, `.Navigation`)
are also available for selective imports.

### Schemas

```ada
--  Schema types (use Tinyaml.Schemas.Prelude)
Str_Schema      -- Any string
Int_Schema      -- Integer (with optional Range_Constraint)
Float_Schema    -- Floating point
Bool_Schema     -- true/false/yes/no/on/off
Enum_Schema     -- One of specified values
Seq_Schema      -- Sequence of items
Map_Schema      -- Mapping with named fields
Any_Schema      -- Accepts any valid YAML node

--  Building a Map_Schema (procedural, dot notation)
Schema : Map_Schema;
Schema.Str ("name");                                    -- Required string
Schema.Str ("nickname", Optional => True);              -- Optional string
Schema.Int ("port", Constraint => (Min => 1, Max => 65535));
Schema.Bool ("debug", Optional => True, Default => "false");
Schema.Enum ("level", (1 => "low", 2 => "high"));
Schema.Seq ("tags", To_Seq_Item (Str_Schema'(null record)));
Schema.Map ("database", Db_Schema);                     -- Nested map
```

### Memory Management

TinyAML provides two approaches for memory management:

**Automatic (recommended):** Use `Document` which is a controlled type that automatically frees the entire node tree when it goes out of scope:

```ada
declare
   Doc : constant Document := Tinyaml.Parser.Parse_Document (Config);
begin
   Put_Line (Get_String (Root (Doc), "database.host"));
end;  --  All nodes freed automatically here
```

**Manual:** Use `Parse` and call `Free_Node` explicitly:

```ada
Doc : Node_Access := Tinyaml.Parser.Parse (Config);
--  ... use Doc ...
Free_Node (Doc);  --  Recursively frees all nodes; sets Doc to null
```

For schemas, `Free_Schema` is available for cleanup when schemas are created dynamically, though schemas are typically static and live for the program's lifetime.

## Building from Source

Requires Ada 2022 compiler (FSF GNAT 13+).

```bash
alr build              # Build library
alr exec -- ./bin/test_runner   # Run tests (after building tests)
```

## License

MIT — see [LICENSE.md](LICENSE.md)
