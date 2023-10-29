import Generate.Common
import Generate.Misc

import Lean.Data.Xml
import Lean.Data.HashMap
import Lean.Data.HashSet

open Lean.Xml
open Lean (HashMap HashSet)

def C.Decl.parse (e : Element) : Option Decl := do
  e.checkVulkanApi

  let ⟨_, _, c⟩ := e
  let mut l := c.toList

  let const : Bool ←
    match ← l.head? with
    | Content.Character "const" =>
      l := ← l.tail?
      pure true
    | _ => pure false

  let struct : Bool ←
    match ← l.head? with
    | Content.Character "struct" =>
      l := ← l.tail?
      pure true
    | _ => pure false

  let type : String ←
    match ← l.head? with
    | Content.Element ⟨"type", _, #[.Character type]⟩ => pure type
    | _ => none
  l := ← l.tail?

  let ptr : Nat ←
    match ← l.head? with
    | Content.Character s =>
      l := ← l.tail?
      pure (s.toList.filter (· = '*')).length
    | _ => pure 0

  let name : String ←
    match ← l.head? with
    | Content.Element ⟨"name", _, #[.Character name]⟩ => pure name
    | _ => none
  l := ← l.tail?

  let array : Option (Option Nat) := do
    if let Content.Character s := ← l.head? then
      if s.startsWith "[" then
        if let some digit := s.drop 1 |>.take 1 |>.toNat? then
          return (some digit)
        else
          return none
    none

  let comment : Option String := do
    match ← l.head? with
    | Content.Element ⟨"comment", _, #[.Character comment]⟩ => pure comment
    | _ => none

  pure {
    type := {name := type, const, struct, ptr, array},
    name,
    comment,
    optional := (e.attr "optional").isEqSome "true",
    len := e.attr "len",
  }

def C.Field.parse (e : Element) : Option Field := do
  let toDecl ← Decl.parse e
  let value := e.attr "values"
  pure {toDecl, value}

def C.Typedef.parse (e : Element) : Option Typedef := do
  e.checkVulkanApi
  let cat ← e.attr "category"
  if cat ≠ "basetype" ∧ cat ≠ "bitmask" then none
  let requires := e.attr "requires"
  match e with
  | ⟨_, _, #[
    .Character "typedef",
    .Element ⟨"type", _, #[.Character type]⟩,
    .Element ⟨"name", _, #[.Character name]⟩,
    .Character ";"
  ]⟩ => some {
      name,
      type := if let some type := requires then type else type,
    }
  | _ => none

def C.Handle.parse (e : Element) : Option Handle := do
  e.checkAttr "category" "handle"
  match ← e.firstChild "name" with
  | ⟨_, _, #[.Character name]⟩ => pure {name}
  | _ => none

def C.Enum.parse (e : Element) : Option Enum := do
  let isBitMask ←
    match ← e.attr "type" with
    | "enum" => some false
    | "bitmask" => some true
    | _ => none
  let mut values := #[]
  for e in e.children "enum" do
    if let some value := (e.attr (if isBitMask then "bitpos" else "value")).bind (·.toInt?) then
      values := values.push (← e.attr "name", value.toNat.toUInt32)
  return {name := ← e.attr "name", values, isBitMask}

def C.Struct.parse (e : Element) : Option Struct := do
  let cat ← e.attr "category"
  if cat ≠ "struct" ∧ cat ≠ "union" then none

  let name ← e.attr "name"

  let fields := (e.children "member").filterMap Field.parse

  pure {name, fields, isUnion := cat = "union"}

def C.Command.parse (e : Element) : Option Command := do
  e.checkVulkanApi

  let proto ← Decl.parse <| ← e.firstChild "proto"
  let params := (e.children "param").filterMap Decl.parse
  let params := params.map (λ d => {toDecl := d})
  pure {proto, params}

def C.FeatureSet.parse (e : Element) : Option FeatureSet := do
  let reqs := e.children "require"
  let name (e : Element) := e.attr "name"
  let names := reqs.map (λ e => ((e.children "type").filterMap name, (e.children "command").filterMap name))
  let (_, commands) := Array.foldl (λ (x, y) (a, b) => (x.append a, y.append b)) (#[], #[]) names
  pure {commands, name := ← e.attr "name"}

def C.Extension.parse (e : Element) : Option Extension := do
  let req ← e.firstChild "require"
  let enumExtensions := req.children "enum" |>.filterMap (λ e => do pure {
    name := ← e.attr "name",
    enumName := ← e.attr "extends",
    offset := (← (← e.attr "offset").toNat?).toUInt32
  })
  pure {
    name := ← e.attr "name",
    number := ← (← e.attr "number").toNat?
    commands := req.children "command" |>.filterMap (·.attr "name"),
    enumExtensions,
  }

def Registry.parse (e : Element) : Option Registry := do
  let types := (← e.firstChild "types").children "type"

  let structs := types.filterMap C.Struct.parse
  let typedefs := types.filterMap C.Typedef.parse
  let handles := types.filterMap C.Handle.parse
  let commands := (← e.firstChild "commands").children "command" |>.filterMap C.Command.parse
  let enums := (e.children "enums").filterMap C.Enum.parse
  let extensions := (← e.firstChild "extensions").children "extension" |>.filterMap C.Extension.parse

  let features :=
    structs.map C.Feature.struct ++
    typedefs.map C.Feature.typedef ++
    handles.map C.Feature.handle ++
    commands.map C.Feature.command ++
    enums.map C.Feature.enum
  let features := HashMap.ofList <| features.toList.map (λ i => (i.name, i))

  let featureSets := (e.children "feature").filterMap C.FeatureSet.parse
  let featureSets := HashMap.ofList <| featureSets.toList.map (λ i => (i.name, i))

  let extensions := HashMap.ofList <| extensions.toList.map (λ i => (i.name, i))

  pure {features, featureSets, extensions}
