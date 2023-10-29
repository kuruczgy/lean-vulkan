import Generate.Registry
import Generate.Compile
import Generate.Emit

import Lean.Data.Xml
import Lean.Data.HashMap
open Lean.Xml

def main : IO Unit := do
  let filename ← (← IO.getEnv "vk_xml_path").unwrap
  let doc := Element.stripWhitespace <| ← IO.ofExcept <| parse <| ← IO.FS.readFile filename
  let reg ← (Registry.parse doc).unwrap

  let featureSets := #["VK_VERSION_1_0"]
  let extensions := #["VK_KHR_surface", "VK_KHR_swapchain"]

  let compiled ← (Registry.compile reg featureSets extensions).unwrap

  let (_, {once := _, lean, c, cInd := _}) := compiled.emit default

  IO.FS.writeFile "src/Vk/Glue.lean" lean
  IO.FS.writeFile "src/Vk/glue.c" c
