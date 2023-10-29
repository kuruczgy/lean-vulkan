import Generate.Common
import Generate.Misc

def L.type.isOmitted (t : type) : Bool :=
  match t with
  | .omitted _ | .arrayLength _ _ => true
  | _ => false

def L.type.scalarBytes (t : type) : Option Nat :=
  match t with
  | .uint bytes => return bytes
  | .float => return 8
  | .alias opaque_ _ of => if opaque_ then none else of.scalarBytes
  | _ => none

def L.type.passedNatively (t : type) : Bool :=
  match t with
  | .uint _ | .float | .enum _ _ _ => true
  | .alias opaque_ _ of => ¬opaque_ ∧ of.passedNatively
  | _ => false

def L.type.isOpaque (t : type) : Bool :=
  match t with
  | .alias opaque_ _ of => opaque_ || of.isOpaque
  | _ => false

def L.type.unalias (t : type) : type :=
  match t with
  | .alias opaque_ _ of => if opaque_ then t else of.unalias
  | _ => t

def L.type.toC (t : type) :=
  match t with
  | .string => "char*"
  | .uint bytes => s!"uint{bytes * 8}_t"
  | .float => "float"
  | .alias _ name _ => name
  | .option t => t.toC
  | .enum name _ _ => name
  | .array elem | .fixedArray _ elem => s!"{elem.toC}*"
  -- | .fixedArray size elem => s!"{elem.toC}[{size}]"
  | .struct name _ => name
  | .union name _ => name
  | .omitted _ => "unknown"
  | .arrayLength _ t => t.toC

-- In C, var should be of type t.toC
partial def L.type.marshal (t : type) (var : String) : EmitM String := do
  match t with
  | .string => return s!"lean_mk_string({var})"
  | .uint bytes => return s!"lean_box_uint{bytes * 8}((uint{bytes * 8}_t){var})"
  | .float => return s!"lean_box_float({var})"
  | .alias _ _ of => of.marshal var
  | .option _ => return "TODO_marshal_option"
  | .enum _ _ _ => return s!"lean_box_uint32((uint32_t){var})"
  | .array elem | .fixedArray _ elem =>
    let len :=
      match t with
      | .fixedArray size _ => s!"{size}"
      | _ => s!"len_{var}"
    match elem.unalias with
    | .uint 1 =>
      Emit.c s!"lean_object *m_{var} = lean_alloc_sarray(1, {len}, {len});"
      Emit.c s!"memcpy(lean_sarray_cptr(m_{var}), {var}, {len});"
    | .float =>
      Emit.c s!"lean_object *m_{var} = lean_alloc_sarray(8, 0, {len});"
      Emit.c s!"for (size_t i = 0; i < {len}; ++i) \{"
      Emit.c s!"  lean_float_array_push(m_{var}, {var}[i]);"
      Emit.c "}"
    | _ =>
      Emit.c s!"lean_object *m_{var} = lean_alloc_array({len}, {len});"
      Emit.c s!"for (size_t i = 0; i < {len}; ++i) \{"
      Emit.c_indent 1
      Emit.c s!"{elem.toC} i_{var} = {var}[i];"
      Emit.c s!"lean_array_cptr(m_{var})[i] = {← elem.marshal s!"i_{var}"};"
      Emit.c_indent (-1)
      Emit.c "}"
    return s!"m_{var}"
  | .struct _ fields =>
    let fields := fields.filter (λ (_, _, t) => ¬t.isOmitted)
    let objs := fields.filter (λ (_, _, t) => t.scalarBytes.isNone)
    let scalars := fields.filterMap (λ f => if let some bytes := f.2.2.scalarBytes then some (bytes, f) else none)
    let scalarSize := scalars.foldl (λ a (bytes, _, _, _) => a + bytes) 0
    Emit.c s!"lean_object *m_{var} = lean_alloc_ctor(0, {objs.size}, {scalarSize});"

    let mut idx := 0
    for (n, _, t) in objs do
      Emit.c s!"{t.toC} {var}_{n} = {var}.{n};";
      Emit.c s!"lean_ctor_set(m_{var}, {idx}, {← t.marshal s!"{var}_{n}"});"
      idx := idx + 1

    let mut offset := 0
    for (bytes, n, _, t) in scalars do
      Emit.c s!"*({t.toC}*)(lean_ctor_scalar_cptr(m_{var}) + {offset}) = {var}.{n};"
      offset := offset + bytes

    return s!"m_{var}"
  | .union _ _ => return s!"TODO_marshal_union"
  | .omitted _ | .arrayLength _ _ => return s!"ERROR_marshal"

def L.Param.marshal (p : Param) : EmitM String := p.type.marshal s!"out_{p.name}"

def L.type.defaultLean (t : type) : Option String :=
  match t with
  | .uint _ => some "0"
  | .alias opaque_ _ of => if opaque_ then none else of.defaultLean
  | .option _ => "none"
  | .enum _ _ true => "default"
  | .array _ | .fixedArray _ _ => ".empty"
  | _ => none

def L.type.sarrayType (t : type) : Option String :=
  match t.unalias with
  | .float => some "FloatArray"
  | .uint 1 => some "ByteArray"
  | _ => none

private def cutVkPrefix (n : String) (lower : Bool := false) := Id.run do
  let parts := n.camelCaseParts
  if let (pre :: h :: t) := parts.toList then
    if pre.toLower = "vk" then
      return String.join <| (if lower then h.toLower else h) :: t
  return n

private def cutFieldPrefix (n : String) := Id.run do
  let parts := n.camelCaseParts
  if let (pre :: h :: t) := parts.toList then
    if pre = "p" ∨ pre = "pp" then
      return String.join <| h.toLower :: t
  return n

private def calculateFieldOffsets (fields : Array (String × Bool × L.type)) : Array Nat := Id.run do
  let fieldsBytes := fields.mapIdx (λ i (_, _, t) => (i, t.scalarBytes))
  let scalarFields := fieldsBytes.filterMap (λ (i, b) => match b with | some b => some (i, b) | _ => none)
  let scalarFields := scalarFields.insertionSort (·.2 > ·.2)
  let mut offset := 0
  let mut scalarOffsets := #[]
  for (i, b) in scalarFields do
    scalarOffsets := scalarOffsets.push (i, offset)
    offset := offset + b
  let fieldOffsets := fields.mapIdx (λ i _ => scalarOffsets.find? (λ (i', _) => i' == i) |>.map (·.2) |>.getD 0)
  fieldOffsets

partial def L.type.toLean (t : type) : EmitM String := do
  match t with
  | .string => return "String"
  | .uint bytes => return s!"UInt{bytes * 8}"
  | .float => return "Float"
  | .alias opaque_ name of =>
    if ← Emit.once name then
      if let .enum _ _ true := of then
        let _ ← of.toLean
        pure ()
      else
        Emit.lean s!"{if opaque_ then "opaque" else "abbrev"} {cutVkPrefix name} : Type := {← of.toLean}"
    return cutVkPrefix name
  | .option t => return s!"Option ({← t.toLean})"
  | .enum name values isBitMask =>
    let mut cutName := cutVkPrefix name
    if isBitMask then
      if cutName.endsWith "Bits" then
        cutName := cutName.dropRight 4 ++ "s"
      else if cutName.endsWith "BitsKHR" then
        cutName := cutName.dropRight 7 ++ "sKHR"
    if ← Emit.once name then
      let prefixName :=
        if name.endsWith "FlagBits" then name.dropRight 8
        else if name.endsWith "FlagBitsKHR" then name.dropRight 11
        else name
      let namePrefixLen := prefixName.camelCaseToSnakeCase.length + 1
      let simplify (n : String) := Id.run do
        let mut n := n.drop namePrefixLen |>.snakeCaseToCamelCase
        if isBitMask ∧ n.endsWith "Bit" then
          n := n.dropRight 3
        if n.front.isDigit then
          n := "_" ++ n
        return n
      Emit.lean s!"structure {cutName} := private mk :: private v : UInt32"
      Emit.lean s!"deriving DecidableEq"
      for (n, v) in values do
        Emit.lean s!"def {cutName}.{simplify n} := mk {v}"
      if isBitMask then
        Emit.lean s!"instance : HOr {cutName} {cutName} {cutName} := ⟨(⟨·.v ||| ·.v⟩)⟩"
        Emit.lean s!"instance : HAnd {cutName} {cutName} Bool := ⟨(·.v &&& ·.v != 0)⟩"
        Emit.lean s!"instance : Inhabited {cutName} := ⟨⟨0⟩⟩"
    return cutName
  | .array elem | .fixedArray _ elem =>
    if let some sarrayType := elem.sarrayType then
      return sarrayType
    return s!"Array ({← elem.toLean})"
  -- | .fixedArray size elem => return s!"FixedArray ({← elem.toLean}) {size}"
  | .struct name fields =>
    if ← Emit.once name then
      let fields ← fields.filterMapM (λ (n, _, t) => do
        if ¬t.isOmitted then
          return s!"  {cutFieldPrefix n} : {← t.toLean}{if let some d := t.defaultLean then s!" := {d}" else ""}"
        else
          return none
      )
      let fields := if fields.size == 1 then fields ++ #["  dummy : Unit := ()"] else fields
      Emit.lean s!"structure {cutVkPrefix name} where\n{"\n".intercalate fields.toList}"
    return cutVkPrefix name
  | .union name fields =>
    if ← Emit.once name then
      let fields ← fields.mapM (λ (n, f) => do return s!"  | {n} (_ : {← f.toLean})")
      Emit.lean s!"inductive {cutVkPrefix name} where\n{"\n".intercalate fields.toList}"
    return cutVkPrefix name
  | .omitted c_val => return s!"(unknown_omitted \"{c_val}\")"
  | .arrayLength _ _ => return "unknown_array_length"

-- In C, var should be of type lean_object *
partial def L.type.unmarshal (t : type) (var : String) : EmitM String := do
  match t with
  | .string => return s!"(char*)lean_string_cstr({var})"
  | .uint bytes => return s!"lean_unbox_uint{bytes * 8}({var})"
  | .float => return s!"(float)lean_unbox_float({var})"
  | .alias _ name of => return s!"({name}){← of.unmarshal var}"
  | .option t =>
    Emit.c s!"_Bool is_some_{var} = !lean_is_scalar({var});"
    Emit.c s!"{t.toC} um_{var};"
    Emit.c s!"if (is_some_{var}) \{"
    Emit.c_indent 1
    Emit.c s!"lean_object *some_{var} = lean_ctor_get({var}, 0);"
    Emit.c s!"um_{var} = {← t.unmarshal s!"some_{var}"};"
    Emit.c_indent (-1)
    Emit.c "}"
    return s!"(is_some_{var} ? &um_{var} : NULL)"
  | .enum name _ _ => return s!"({name})lean_unbox_uint32({var})"
  | .array elem | .fixedArray _ elem =>
    Emit.c s!"size_t len_{var} = lean_{if elem.sarrayType.isSome then "s" else ""}array_size({var});"
    match elem.unalias with
    | .uint 1 =>
      Emit.c s!"void *um_{var} = lean_sarray_cptr({var});"
    | .float =>
      Emit.c s!"float* um_{var} = calloc(len_{var}, sizeof(float));"
      Emit.c s!"for (size_t i = 0; i < len_{var}; ++i) \{"
      Emit.c s!"  um_{var}[i] = lean_float_array_uget({var}, i);"
      Emit.c "}"
    | _ =>
      Emit.c s!"{elem.toC}* um_{var} = calloc(len_{var}, sizeof({elem.toC}));"
      Emit.c s!"for (size_t i = 0; i < len_{var}; ++i) \{"
      Emit.c_indent 1
      Emit.c s!"lean_object *i_{var} = lean_array_cptr({var})[i];"
      Emit.c s!"um_{var}[i] = {← elem.unmarshal s!"i_{var}"};"
      Emit.c_indent (-1)
      Emit.c "}"
    match t with
    | .fixedArray size _ =>
      Emit.c s!"if (len_{var} != {size}) abort();"
      let mut s := "{"
      for i in [:size] do
        s := s ++ s!"um_{var}[{i}],"
      return s ++ "}"
    | _ => return s!"um_{var}"
  | .struct name fields =>
    let mut stmt := s!"struct {name} um_{var} = \{\n"
    let mut idx := 0
    let fieldOffsets := calculateFieldOffsets fields
    for (offset, n, byRef, t) in Array.zip fieldOffsets fields do
      if let .arrayLength arrays _ := t then
        stmt := stmt ++ s!"{← Emit.ind}  .{n} = len_{var}_{arrays[0]?.getD ""},\n"
        continue
      if let .omitted c_value := t then
        stmt := stmt ++ s!"{← Emit.ind}  .{n} = {c_value},\n"
        continue
      if let some bytes := t.scalarBytes then
        let readType := match t with | .float => "double" | _ => s!"uint{bytes * 8}_t"
        stmt := stmt ++ s!"{← Emit.ind}  .{n} = ({t.toC})*({readType}*)((uint8_t*)(lean_ctor_obj_cptr({var}) + lean_ctor_num_objs({var})) + {offset}),\n"
      else
        Emit.c s!"lean_object *{var}_{n} = lean_ctor_get({var}, {idx});"
        stmt := stmt ++ s!"{← Emit.ind}  .{n} = {if byRef then "&" else ""}{← t.unmarshal s!"{var}_{n}"},\n"
        idx := idx + 1
    stmt := stmt ++ (← Emit.ind) ++ "};"
    Emit.c stmt
    return s!"um_{var}"
  | .union name fields =>
    Emit.c s!"union {name} um_{var};"
    Emit.c s!"switch (lean_ptr_tag({var})) \{"
    let mut idx := 0
    for (n, t) in fields do
      Emit.c s!"case {idx}: \{"
      Emit.c_indent 1
      if let some _ := t.scalarBytes then
        Emit.c s!"  um_{var}.{n} = *({t.toC}*)lean_ctor_obj_cptr({var});"
      else
        Emit.c s!"lean_object *{var}_{n} = lean_ctor_get({var}, 0);"
        Emit.c s!"um_{var} = (union {name})\{ .{n} = {← t.unmarshal s!"{var}_{n}"} };"
      Emit.c_indent (-1)
      Emit.c s!"} break;"
      idx := idx + 1
    Emit.c "}"
    return s!"um_{var}"
  | .omitted c_value => return c_value
  | .arrayLength _ _ => return s!"len_unknown"

partial def L.Param.unmarshal (p : Param) : EmitM String := do
  match p.dir with
  | .input =>
    if let .arrayLength arrays _ := p.type then
      return s!"len_{arrays[0]?.getD ""}"
    let ref ← (do
      if p.type.passedNatively then
        return p.name
      else
        return (← p.type.unmarshal p.name))
    return (if p.byRef then "&" else "") ++ ref
  | .output =>
    let mut var := s!"out_{p.name}"
    if let .arrayLength arrays _ := p.type then
      var := s!"len_out_{arrays[0]?.getD ""}"
    Emit.c s!"{p.type.toC} {var};"
    return s!"{if p.byRef then "&" else ""}{var}"

partial def L.Param.toC (p : Param) : String :=
  match p.type with
  | .uint bytes => s!"uint{bytes * 8}_t"
  | .float => "double"
  | .alias _ name _ =>
    if p.type.passedNatively then name else "b_lean_obj_arg"
  | .enum name _ _ => name
  | _ => "b_lean_obj_arg"

def L.Command.emit (c : Command) : EmitM Unit := do
  let leanInputs := c.params.filter (λ p => match p.dir with | .input => ¬p.type.isOmitted | _ => false)
  let leanInputsType := " → ".intercalate <| ← leanInputs.toList.mapM (λ p => do return s!"(@& {← p.type.toLean})")

  let retParam : L.Param := {
    name := "ret",
    type := c.ret,
    dir := .output,
    byRef := false,
  }
  let outputs := (#[retParam] ++ c.params).filter (λ p => match p.dir with | .output => ¬p.type.isOmitted | _ => false)
  let mut leanOutType := " × ".intercalate <| ← outputs.toList.mapM (·.type.toLean)
  if outputs.size = 0 then leanOutType := "Unit"
  if outputs.size > 1 ∨ leanOutType.contains ' ' then leanOutType := s!"({leanOutType})"

  Emit.lean s!"@[extern \"glue_{c.name}\"]"
  Emit.lean s!"opaque {cutVkPrefix c.name true} : {leanInputsType}{if leanInputs.size > 0 then " → " else ""}IO {leanOutType}"
  Emit.lean ""

  let glueParams := ", ".intercalate <| leanInputs.toList.map (λ p => s!"{p.toC} {p.name}")

  Emit.c s!"LEAN_EXPORT lean_obj_res glue_{c.name}({glueParams}{if leanInputs.size > 0 then ", " else ""}b_lean_obj_arg w) \{"
  Emit.c_indent 1

  let unmarshaledParams ← c.params.toList.mapM (·.unmarshal)

  let commandParams (without : Option String := none) :=
    ", ".intercalate <| unmarshaledParams.map (λ p =>
      if without.map (p != ·) |>.getD true then p else "NULL"
    )

  -- Hack: special cases
  match c.name with
  | "vkAllocateCommandBuffers" =>
    Emit.c "uint32_t len_out_pCommandBuffers = um_pAllocateInfo.commandBufferCount;"
    Emit.c "out_pCommandBuffers = calloc(len_out_pCommandBuffers, sizeof(VkCommandBuffer));"
  | "vkAllocateDescriptorSets" =>
    Emit.c "uint32_t len_out_pDescriptorSets = um_pAllocateInfo.descriptorSetCount;"
  | "vkCreateGraphicsPipelines" =>
    Emit.c "out_pPipelines = calloc(len_pCreateInfos, sizeof(VkPipeline));"
  | _ => pure ()

  -- Handle case where there is an output whose length is determined by an input
  for param in c.params do
    if let .arrayLength arrayNames t := param.type then
      let arrays := c.params.filter (λ p => arrayNames.contains p.name)
      let inputArrays := arrays.filter (λ p => p.dir == .input)
      let outputArrays := arrays.filter (λ p => p.dir == .output)
      if let (#[input], #[output]) := (inputArrays, outputArrays) then
        Emit.c s!"{t.toC} len_out_{output.name} = len_{input.name};";

  -- Handle the "two-phase" pattern where we first get the length, allocate a
  -- buffer, then fill it with a second call.
  if let some (len, #[arr]) := c.params.filterMap (λ p => if let .arrayLength n _ := p.type then pure (p, n) else none) |>.back? then
    if let some arr := c.params.find? (λ p => if let .output := p.dir then arr = p.name else false) then
      if let .array elem := arr.type then
        Emit.c s!"// get length {len.name} of {arr.name}"
        Emit.c s!"{c.name}({commandParams s!"out_{arr.name}"});"
        Emit.c s!"out_{arr.name} = calloc(len_out_{arr.name}, sizeof({elem.toC}));"

  let resultBinding :=
    match c.ret with
    | .omitted _ => ""
    | _ => s!"{c.ret.toC} out_ret = "

  Emit.c s!"{resultBinding}{c.name}({commandParams});"

  match outputs.toList.reverse with
  | [] => Emit.c "return lean_io_result_mk_ok(lean_box(0));"
  | h :: t =>
    Emit.c s!"lean_object *temp, *tuple = {← h.marshal};"
    for p in t do
      Emit.c s!"temp = lean_alloc_ctor(0, 2, 0);"
      Emit.c s!"lean_ctor_set(temp, 0, {← p.marshal});"
      Emit.c s!"lean_ctor_set(temp, 1, tuple);"
      Emit.c s!"tuple = temp;"
    Emit.c "return lean_io_result_mk_ok(tuple);"

  Emit.c_indent (-1)
  Emit.c "}"


def Compiled.emit (com : Compiled) : EmitM Unit := do
  Emit.lean "set_option autoImplicit false"
  Emit.lean "namespace Vk"
  Emit.lean "abbrev FixedArray α (_ : Nat) := Array α"

  Emit.c "#include <stdlib.h>"
  Emit.c "#include <vulkan/vulkan_core.h>"
  Emit.c "#include <lean/lean.h>"
  Emit.c "#include <stdio.h> // for debugging"
  Emit.c ""
  Emit.c "#define Pointer void*"
  Emit.c ""

  com.commands.forM (·.emit)

  Emit.lean "end Vk"
