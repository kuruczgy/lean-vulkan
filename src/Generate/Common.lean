import Lean.Data.HashMap
import Lean.Data.HashSet

open Lean (HashMap HashSet)

namespace C

/-- Any vulkan feature that is identified by a name. -/
structure Named where
  name : String
deriving Repr

structure type extends Named where
  const : Bool := false
  struct : Bool := false
  ptr : Nat := 0
  array : Option (Option Nat) := none
deriving Repr

/-- Base type for a struct field, function parameter, or function signature. -/
structure Decl extends Named where
  type : type
  comment : Option String := none
  optional : Bool := false
  len : Option String := none
deriving Repr

/-- Field of a struct. -/
structure Field extends Decl where
  /-- The only possible value for this field. If present, it can be automatically filled. -/
  value : Option String := none
deriving Repr

structure Struct extends Named where
  fields : Array Field
  isUnion : Bool
deriving Repr

structure Typedef extends Named where
  type : String

/-- All handles are just pointers underneath. -/
structure Handle extends Named where

structure Enum extends Named where
  values : Array (String × UInt32)
  isBitMask : Bool

/-- Parameter of a command. -/
structure Param extends Decl where

/-- Represents a vulkan command. -/
structure Command where
  proto : Decl
  params : Array Param

inductive Feature where
  | struct (_ : Struct)
  | enum (_ : Enum)
  | command (_ : Command)
  | handle (_ : Handle)
  | typedef (_ : Typedef)
def Feature.name : Feature → String
  | struct s => s.name
  | enum e => e.name
  | command c => c.proto.name
  | handle h => h.name
  | typedef t => t.name

structure FeatureSet extends Named where
  commands : Array String

structure EnumExtension extends Named where
  enumName : String
  offset : UInt32

structure Extension extends Named where
  number : Nat
  commands : Array String
  enumExtensions : Array EnumExtension

end C

namespace L

inductive type where
  | string
  | uint (bytes : Nat)
  | float
  | alias (opaque_ : Bool) (name : String) (of : type)
  | option (t : type)
  | enum (name : String) (values : Array (String × UInt32)) (isBitMask : Bool)
  | array (elem : type)
  | fixedArray (size : Nat) (elem : type)
  | struct (name : String) (fields : Array (String × Bool × type))
  | union (name : String) (fields : Array (String × type))
  | omitted (c_value : String)
  /-- nameOfArray refers to either another struct field or parameter -/
  | arrayLength (arrays : Array String) (t : type)
deriving Inhabited

inductive Direction where
  | input
  | output
deriving DecidableEq

structure Param where
  name : String
  type : type
  dir : Direction
  byRef : Bool

structure Command where
  name : String
  params : Array Param
  ret : type

end L

/-- Represents the raw data parsed from vk.xml. -/
structure Registry where
  features : HashMap String C.Feature
  featureSets : HashMap String C.FeatureSet
  extensions : HashMap String C.Extension

structure Compiled where
  commands : Array L.Command

structure EmitState where
  once : HashSet String
  lean : String
  c : String
  cInd : Int
deriving Inhabited

abbrev EmitM := StateT EmitState Id

def String.repeat (s : String) (i : Nat) :=
  match i with
  | .zero => ""
  | .succ i' => s ++ s.repeat i'

namespace Emit
def lean (s : String) : EmitM Unit :=
  modify (λ g => {g with lean := g.lean ++ s ++ "\n"})
def c (s : String) : EmitM Unit :=
  modify (λ g => {g with c := g.c ++ "  ".repeat g.cInd.toNat ++ s ++ "\n"})
def c_indent (i : Int) : EmitM Unit :=
  modify (λ g => {g with cInd := g.cInd + i})
def ind : EmitM String := do return "  ".repeat (← get).cInd.toNat
def once (k : String) : EmitM Bool :=
  modifyGet (λ g => (¬g.once.contains k, {g with once := g.once.insert k}))
end Emit
