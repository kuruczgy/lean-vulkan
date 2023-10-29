import Lean.Data.Xml
open Lean.Xml

namespace Option
def unwrap [Monad m] [MonadExcept IO.Error m] (o : Option α) (error : IO.Error := .userError "unwrap failed") : m α :=
  match o with
  | some v => pure v
  | none => throw error
end Option

mutual
private partial def eStripWs : Element → Element
| .Element n a c => .Element n a (c.filterMap cStripWs)
private partial def cStripWs : Content → Option Content
| .Element e => pure <| .Element <| eStripWs e
| .Comment _ => none
| .Character c =>
  let trimmed := c.trim
  if trimmed ≠ "" then
    some (.Character trimmed)
  else
    none
end

def ElemPred := Element → Bool

def tagp (tag : String) (e : Element) :=
  let ⟨t, _, _⟩ := e
  t == tag

instance : Coe String ElemPred := { coe := tagp }

namespace Lean.Xml.Element
variable (e : Xml.Element)
def stripWhitespace := eStripWs
def attr (k : String) :=
  let ⟨_, a, _⟩ := e
  a.find? k

def children (p : ElemPred) : Array Xml.Element :=
  let ⟨_, _, content⟩ := e
  Array.filterMap (λ c =>
    match c with
    | .Element e => if p e then some e else none
    | .Comment _ | .Character _ => none
  ) content

-- TODO: efficiency
def firstChild p := (children e p)[0]?

def checkVulkanApi : Option Unit :=
  match e.attr "api" with
  | none | some "vulkan" => pure ()
  | _ => none

def checkAttr (k : String) (v : String) : Option Unit :=
  if (e.attr k).isEqSome v then pure () else none

end Lean.Xml.Element

def String.camelCaseParts (s : String) := Id.run do
  let mut parts : Array String := #[]
  for c in s.toList do
    if let some last := parts.back? then
      if c.isUpper ∧ ¬last.back.isUpper then
        parts := parts.push c.toString
      else
        parts := parts.pop.push (last ++ c.toString)
    else
        parts := parts.push c.toString
  return parts

def String.snakeCaseParts (s : String) := s.split (· = '_')

def String.camelCaseToSnakeCase (s : String) :=
  let parts := s.camelCaseParts
  "_".intercalate <| parts.toList.map (·.toLower)

def String.snakeCaseToCamelCase (s : String) :=
  if let (h :: t) := s.snakeCaseParts then
    String.join <| h.toLower :: t.map (String.capitalize <| String.toLower ·)
  else
    s
