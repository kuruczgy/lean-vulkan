import Lake
open Lake DSL

package generate  {
  srcDir := "src"
  moreLinkArgs := #["-lvulkan", "-lglfw"]
}

lean_lib Generate

lean_lib Vk

@[default_target]
lean_exe generate {
  root := `Generate
}

@[default_target]
lean_exe demo {
  root := `Demo
}

target vk.o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "vk.o"
  let srcJob ← inputFile <| pkg.srcDir / "Vk" / "glue.c"
  let flags := #["-I", (← getLeanIncludeDir).toString, "-fPIC"]
  buildO "glue.c" oFile srcJob flags

target glfw.o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "glfw.o"
  let srcJob ← inputFile <| pkg.srcDir / "Vk" / "glfw.c"
  let flags := #["-I", (← getLeanIncludeDir).toString, "-fPIC"]
  buildO "glfw.c" oFile srcJob flags

extern_lib vk_glue pkg := do
  let name := nameToStaticLib "vk_glue"
  let vkO ← fetch <| pkg.target ``vk.o
  let glfwO ← fetch <| pkg.target ``glfw.o
  buildStaticLib (pkg.nativeLibDir / name) #[vkO, glfwO]
