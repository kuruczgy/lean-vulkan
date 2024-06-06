# Lean Vulkan

This project is a proof of concept for writing Vulkan code in Lean. The only thing that works right now is drawing a single triangle.

Simplifications are made to the API wherever possible, for example by using namespaces and automatically filling some values. For example this

```c
const char *extensions[] = {"VK_EXT_debug_utils"};
const char *validationLayers[] = {"VK_LAYER_KHRONOS_validation"};
VkInstanceCreateInfo instanceCreateInfo = {
  .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
  .enabledExtensionCount = 1,
  .ppEnabledExtensionNames = extensions,
  .enabledLayerCount = 1,
  .ppEnabledLayerNames = validationLayers,
};
VkInstance inst;
VkResult res = vkCreateInstance(&instanceCreateInfo, NULL, &inst);
```

turns into this:

```lean
let (res, inst) ← Vk.createInstance {
  enabledExtensionNames := #["VK_EXT_debug_utils"]
  enabledLayerNames := #["VK_LAYER_KHRONOS_validation"],
}
```

## Demo

See [`Demo.lean`](/src/Demo.lean) for an example that renders a single triangle.

To build and run:

- If on non-NixOS, uncomment the `patchelf` line in `fixupPhase`, and comment the `wrapProgram` line.
- `nix build`
- `./result/bin/demo`

And you of course get the well known "Hello World" triangle:

![](https://github.com/kuruczgy/lean-vulkan/blob/images/triangle.png)

## Implementation

This repo contains a generator written in Lean that generates some C and Lean code for binding the Vulkan APIs to Lean. Instead of emitting Lean code the required Lean definitions could also probably be produced using metaprogramming.

First, the Vulkan XML specification is parsed. (See [`Registry.lean`](/src/Generate/Registry.lean).)

Next, the commands from the registry are compiled into a representation that's more amenable to generating the bindings. (See [`Compile.lean`](/src/Generate/Compile.lean).) This roughly involves:

- Translating C types to Lean types. (E.g. Figuring out whether a pointer is just a pass-by-reference, or an `Array`, or an `Option`.)
- Figuring out which parameters are inputs and outputs.
- Figuring out which parameters/fields are array lengths. (In the case of parameters these can be both inputs and outputs at the same time!)
- Computing enum and bitfield values.
- Omitting some fields and filling them with default values. (E.g. all the `sType`, `pNext`, and `pAllocator` fields.)
- Applying some special case transformations.

Finally, [`Emit.lean`](/src/Generate/Emit.lean) generates the C and the Lean code.

The generated Lean code just consists of type definitions, it's relatively simple.

The C side is more complicated. For each command, code is generated to recursively unmarshal Lean values into C ones, which can then be passed to the actual Vulkan commands. Similarly, code is generated for marshaling back the outputs of the command to Lean values, which can then be returned to the caller.

You can check out what the generated code looks like here: https://gist.github.com/kuruczgy/c6bfd5d4efa4e3bbf56e4a87c9deaf56

## Improvement ideas (TODOs)

- Fix memory leak by actually `free`ing the memory allocated during unmarshaling.
- Figure out how checking result codes should work on the Lean side.
- Add newer Vulkan APIs than 1.0.
- Fix name simplification for extension identifiers.
- Translate `VkBool32` to `Bool`.
- Handle multiple fields with the same length.
- Generally clean up messy logic, replace with special cases where that makes more sense.
- Emit Lean type definitions using metaprogramming. (Also could we query Lean for the layout of the structures?)
- Emit C glue code with lake facets somehow?
