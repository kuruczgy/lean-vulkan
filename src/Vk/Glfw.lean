import Vk.Glue

namespace Glfw

opaque Window : Type := UInt64

@[extern "glue_glfw_init"]
opaque init : IO Unit

@[extern "glue_glfw_extensions"]
opaque extensions : IO (Array String)

@[extern "glue_glfw_create_window"]
opaque createWindow : IO Window

@[extern "glue_glfw_create_window_surface"]
opaque createWindowSurface : Vk.Instance → Window → IO Vk.SurfaceKHR

@[extern "glue_glfw_get_framebuffer_size"]
opaque getFramebufferSize : Window → IO Vk.Extent2D

@[extern "glue_glfw_run"]
opaque run : Window → (draw : @& IO Unit) → IO Unit

end Glfw
