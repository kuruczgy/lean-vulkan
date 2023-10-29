import Generate.Registry

open Lean (HashSet)

def omittedRootFeatures := #[
  "vkGetQueryPoolResults",
  "vkGetPhysicalDeviceMemoryProperties",
  "vkGetPipelineCacheData",
  "vkCmdSetBlendConstants"
]
def specialCaseFields : Array (String × String × L.type) := #[
  ("VkSubpassDescription", "pResolveAttachments", .omitted "NULL"),
  ("VkSwapchainCreateInfoKHR", "oldSwapchain", .omitted "NULL"),

  ("VkShaderModuleCreateInfo", "codeSize", .omitted "len_pCreateInfo_pCode"),
  ("VkShaderModuleCreateInfo", "pCode", .array (.uint 1)),

  ("VkPipelineMultisampleStateCreateInfo", "pSampleMask", .omitted "NULL"),

  ("VkGraphicsPipelineCreateInfo", "basePipelineHandle", .omitted "VK_NULL_HANDLE"),
  ("VkGraphicsPipelineCreateInfo", "basePipelineIndex", .omitted "-1")

  -- ("VkSemaphoreCreateInfo", "flags", .omitted "0")
]
def specialCaseParams : Array (String × String × L.Param) := #[
  ("vkCreateGraphicsPipelines", "pipelineCache", {
    name := "pipelineCache",
    type := .omitted "VK_NULL_HANDLE",
    dir := .input,
    byRef := false,
  }),
  ("vkAcquireNextImageKHR", "fence", {
    name := "fence",
    type := .omitted "VK_NULL_HANDLE",
    dir := .input,
    byRef := false,
  })
]

private structure Env where
  reg : Registry
  extensions : Array C.Extension

private partial def topologicalSort [BEq α] [Hashable α] (roots : Array α) (edges : α → Array α) : Array α :=
  let rec visit (acc : Array α × HashSet α) (x : α) :=
    if acc.2.contains x then
      acc
    else
      let acc := Array.foldl visit (acc.1, acc.2.insert x) (edges x)
      (acc.1.push x, acc.2)
  Array.foldl visit default roots |>.1

def featureDeps (reg : Registry) (key : String) : Array String :=
  match reg.features.find? key with
  | some f =>
    match f with
    | .struct s => s.fields.map (λ f => f.type.name)
    | .enum _ => #[]
    | .command c => #[c.proto.type.name] ++ c.params.map (λ p => p.type.name)
    | .handle _ => #[]
    | .typedef t => #[t.type]
  | none => #[]

def C.Enum.compile (e : Enum) (env : Env) : L.type := Id.run do
  let mut values := e.values.map (λ (n, v) => (n, if e.isBitMask then 1 <<< v else v))
  for extension in env.extensions do
    for {name, enumName, offset} in extension.enumExtensions do
      if enumName ≠ e.name then continue
      let v := 1000000000 + (extension.number.toUInt32 - 1) * 1000 + offset
      values := values.push (name, v)
  return .enum e.name values e.isBitMask

def C.Handle.compile (h : Handle) : L.type := .alias true h.name (.uint 8)

private def lenPropMatch (n s : String) : Bool := s.takeWhile (· ≠ ',') = n

mutual
partial def C.type.compile (t : type) (isArray : Bool) (env : Env) : Option L.type := do
  if t.ptr = 2 ∧ isArray ∧ t.name = "char" then
    return .array .string
  if isArray then do
    if let some elem := {t with ptr := 0}.compile false env then
      return .array elem
  let wrapArray res : L.type :=
    match t.array with
    | none => res
    | some none => .array res
    | some (some size) => .fixedArray size res
  match t with
  | {name := "VkResult", const := _, struct := _, ptr := 0, array := none} =>
    return .alias false "VkResult" (.uint 8)
  | {name := "void", const := _, struct := _, ptr := 0, array := none} => none
  | {name := "void", const := _, struct := _, ptr := 1, array := none} =>
    if isArray then
      return .array (.uint 1)
    else
      return .alias true "Pointer" (.uint 8)
  | {name := "char", const := _, struct := _, ptr := 0, array := none} => none
  | {name := "char", const := _, struct := false, ptr := 1, array := none} =>
    return .string
  | {name := "uint8_t", const := _, struct := false, ptr := 0, array := none} =>
    return .uint 1
  | {name := "uint32_t", const := _, struct := false, ptr := 0, array := _}
  | {name := "int32_t", const := _, struct := false, ptr := 0, array := _} =>
    return wrapArray <| .uint 4
  | {name := "uint64_t", const := _, struct := false, ptr := 0, array := none}
  | {name := "size_t", const := _, struct := false, ptr := 0, array := none} =>
    return .uint 8
  | {name := "float", const := _, struct := false, ptr := 0, array := _} =>
    return wrapArray .float
  | {name, const := _, struct := _, ptr := _, array} =>
    let mut name := name
    if let some feat := env.reg.features.find? name then
      let res : L.type ← do
        match feat with
        | .struct s => return s.compile env
        | .enum e => return e.compile env
        | .handle h => return h.compile
        | .typedef td =>
          if let some t := C.type.compile {name := td.type, const := false, struct := false, ptr := 0, array := none} false env then
            return .alias false td.name t
        | _ => none
        none
      return match array with
        | none => res
        | some none => .array res
        | some (some size) => .fixedArray size res
  none

partial def C.Struct.compile (s : Struct) (env : Env) : L.type :=
  let fields := s.fields.map (λ field => Id.run do
    if let some (_, _, t) := specialCaseFields.find? (λ (sname, fname, _) => sname = s.name ∧ fname = field.name) then
      return (field.name, false, t)
    if field.name = "sType" ∨ field.name = "pNext" then
      return (field.name, false, .omitted <| field.value.getD "0")
    if let some type := field.type.compile field.len.isSome env then
      let lenOf := s.fields.filter (·.len.map (lenPropMatch field.name) |>.getD false)
      if lenOf.size > 0 then
        return (field.name, false, .arrayLength (lenOf.map (·.name)) type)
      let mut byRef := false
      let mut type := type
      match type with
      | .struct _ _ | .union _ _ =>
        if field.type.ptr > 0 then
          if field.optional then
            type := .option type
          else
            byRef := true
      | _ => pure ()
      return (field.name, byRef, type)
    (field.name, false, .omitted "unknown")
  )
  if s.isUnion then
    .union s.name <| fields.map (λ (n, _, t) => (n, t))
  else
    .struct s.name fields
end

def C.Command.compile (c : Command) (env : Env) : Option L.Command := do
  let params := c.params.map (λ param => Id.run do
    if let some (_, _, p) := specialCaseParams.find? (λ (sname, fname, _) => sname = c.proto.name ∧ fname = param.name) then
      return p

    if param.name = "pAllocator" then
      return {
        name := param.name,
        type := .omitted "NULL",
        dir := .input,
        byRef := false,
      }

    let mut adjustedType := param.type
    let mut dir := .input
    if param.type.ptr > 0 && ¬param.type.const then
      if param.len.isNone then
        adjustedType := {param.type with ptr := param.type.ptr - 1}
      dir := .output

    if let some type := adjustedType.compile param.len.isSome env then
      let lenOf := c.params.filter (·.len.map (lenPropMatch param.name) |>.getD false)
      if lenOf.size > 0 then
        return {
          name := param.name,
          type := .arrayLength (lenOf.map (·.name)) type,
          dir := if param.type.ptr > 0 then .output else .input,
          byRef := param.type.ptr > 0,
        }

      let byRef : Bool :=
        (match dir with | .output => param.len.isNone | _ => false) ||
        match type with | .struct _ _ | .union _ _ => adjustedType.ptr > 0 | _ => false

      -- if param.optional then
      --   -- Handle a handle being optional
      --   if let .alias true _ (.uint 8) := type then
      --     return { name := param.name, type := .option type, dir, byRef }
      return { name := param.name, type, dir, byRef }
    pure { name := param.name, type := .omitted "unknown", dir := .input, byRef := false }
  )
  pure {
    name := c.proto.name,
    params,
    ret := c.proto.type.compile false env |>.getD (.omitted "unknown_ret"),
  }

def Registry.compile (reg : Registry) (featureSets : Array String) (extensions : Array String) : Option Compiled := do
  let extensions := ← extensions.mapM (reg.extensions.find? ·)

  let rootFeatures := (← featureSets.mapM reg.featureSets.find?).concatMap (λ fs => fs.commands)
  let rootFeatures := rootFeatures ++ extensions.concatMap (·.commands)
  let rootFeatures := rootFeatures.filter (λ n => !omittedRootFeatures.contains n)

  let features := topologicalSort rootFeatures (featureDeps reg) |>.filterMap reg.features.find?

  let mut commands := #[]
  for feature in features do
    if let .command c := feature then
      commands := commands.push c

  pure { commands := ← commands.mapM (·.compile {reg, extensions}) }

