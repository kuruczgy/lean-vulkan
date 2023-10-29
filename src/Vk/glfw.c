#include <lean/lean.h>
#include <stdlib.h>
#include <vulkan/vulkan_core.h>
#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

LEAN_EXPORT lean_obj_res glue_glfw_init(b_lean_obj_arg w) {
  glfwInit();
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

  return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res glue_glfw_extensions(b_lean_obj_arg w) {
  uint32_t n;
  const char **exts = glfwGetRequiredInstanceExtensions(&n);

  lean_object *arr = lean_alloc_array(0, n);
  for (size_t i = 0; i < n; ++i) {
    lean_array_push(arr, lean_mk_string(exts[i]));
  }

  return lean_io_result_mk_ok(arr);
}

LEAN_EXPORT lean_obj_res glue_glfw_create_window(b_lean_obj_arg w) {
  GLFWwindow *window = glfwCreateWindow(800, 600, "Vulkan", NULL, NULL);
  return lean_io_result_mk_ok(lean_box_uint64((uint64_t)window));
}

LEAN_EXPORT lean_obj_res glue_glfw_create_window_surface(b_lean_obj_arg _inst,
                                                         b_lean_obj_arg _window,
                                                         b_lean_obj_arg w) {
  VkInstance inst = (VkInstance)lean_unbox_uint64(_inst);
  GLFWwindow *window = (GLFWwindow *)lean_unbox_uint64(_window);
  VkSurfaceKHR surface;
  glfwCreateWindowSurface(inst, window, NULL, &surface);
  return lean_io_result_mk_ok(lean_box_uint64((uint64_t)surface));
}

LEAN_EXPORT lean_obj_res glue_glfw_get_framebuffer_size(b_lean_obj_arg _window,
                                                        b_lean_obj_arg w) {
  GLFWwindow *window = (GLFWwindow *)lean_unbox_uint64(_window);
  int width, height;
  glfwGetFramebufferSize(window, &width, &height);

  lean_object *res = lean_alloc_ctor(0, 0, 8);
  lean_ctor_set_uint32(res, 0, width);
  lean_ctor_set_uint32(res, 4, height);

  return lean_io_result_mk_ok(res);
}

LEAN_EXPORT lean_obj_res glue_glfw_run(b_lean_obj_arg _window,
                                       b_lean_obj_arg draw, b_lean_obj_arg w) {
  GLFWwindow *window = (GLFWwindow *)lean_unbox_uint64(_window);
  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();
    lean_inc(draw);
    lean_apply_1(draw, w);
  }

  return lean_io_result_mk_ok(lean_box(0));
}
