import Vk.Glue
import Vk.Glfw

import Lean.Elab.Eval

-- Get shader_path at compile time
syntax (name := includeEnv) "include_env " term : term
open Lean Elab Term in
@[term_elab includeEnv]
unsafe def elabIncludeEnv : TermElab
  | `(include_env $var:term), _ => do
    let var ← evalTerm String (.const ``String []) var
    let some value ← IO.getEnv var
      | throwError "env var not found"
    return mkStrLit value
  | _, _ => throwUnsupportedSyntax
def shaderPath := include_env "shader_path"

open Vk in
def main : IO Unit := do
  let error s : IO Unit := throw (IO.userError s)

  Glfw.init

  let window ← Glfw.createWindow

  let (_, inst) ← createInstance {
    applicationInfo := some {
      applicationName := "Demo",
      applicationVersion := 0,
      engineName := "No Engine",
      engineVersion := 0,
      apiVersion := 4194304,
    },
    enabledExtensionNames := ← Glfw.extensions
    enabledLayerNames := #[
      -- "VK_LAYER_LUNARG_api_dump",
      "VK_LAYER_KHRONOS_validation"
    ],
  }

  let surface ← Glfw.createWindowSurface inst window

  let (_, devices) ← enumeratePhysicalDevices inst
  let devices ← devices.mapM (λ d => do pure (d, ← getPhysicalDeviceProperties d))
  let some (physicalDevice, _) :=
    devices.find? (λ (_, props) => match props.deviceType with | .discreteGpu | .integratedGpu => true | _ => false)
    | error "Can't find GPU"

  let (_, surfaceCapabilities) ← getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  let (_, formats) ← getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
  let some chosenFormat := formats.find? (λ f => f.format == .b8g8r8a8Srgb && f.colorSpace == .NonlinearKhr)
    | error "Can't find suitable format"

  let (_, presentModes) ← getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface
  let some chosenPresentMode := presentModes.find? (λ m => m == .boxKhr)
    | error "Can't find suitable present mode"

  let queueFamilies ← getPhysicalDeviceQueueFamilyProperties physicalDevice
  let queueFamilySuitability ← queueFamilies.mapIdxM (λ i f => do
    let i := i.val.toUInt32
    let (_, presentSupport) ← getPhysicalDeviceSurfaceSupportKHR physicalDevice i surface
    return (i, presentSupport != 0 && f.queueFlags &&& QueueFlags.graphics)
  )
  let some (queueFamilyIndex, _) := queueFamilySuitability.find? (·.2)
    | error "Can't find suitable queue family"

  let (_, device) ← createDevice physicalDevice {
    queueCreateInfos := #[{
      queueFamilyIndex,
      queuePriorities := [1].toFloatArray
    }],
    enabledExtensionNames := #["VK_KHR_swapchain"],
  }

  let queue ← getDeviceQueue device queueFamilyIndex 0

  let extent ← Glfw.getFramebufferSize window

  let (_, swapchain) ← createSwapchainKHR device {
    surface,
    minImageCount := surfaceCapabilities.minImageCount + 1,
    imageFormat := chosenFormat.format,
    imageColorSpace := chosenFormat.colorSpace,
    imageExtent := extent,
    imageArrayLayers := 1,
    imageUsage := .colorAttachment,
    imageSharingMode := .exclusive,
    preTransform := surfaceCapabilities.currentTransform,
    compositeAlpha := .opaqueBitKhr,
    presentMode := chosenPresentMode,
    clipped := 1,
  }

  let (_, swapChainImages) ← getSwapchainImagesKHR device swapchain

  let swapChainImageViews ← swapChainImages.mapM (λ image => do
    let (_, imageView) ← createImageView device {
      image,
      viewType := ._2d,
      format := chosenFormat.format,
      components := ⟨.identity, .identity, .identity, .identity⟩,
      subresourceRange := {
        aspectMask := .color,
        levelCount := 1,
        layerCount := 1,
      }
    }
    pure imageView
  )

  let (_, renderPass) ← createRenderPass device {
    attachments := #[{
      format := chosenFormat.format,
      samples := ._1,
      loadOp := .clear,
      storeOp := .store,
      stencilLoadOp := .dontCare,
      stencilStoreOp := .dontCare,
      initialLayout := .undefined,
      finalLayout := .presentSrcKhr,
    }]
    subpasses := #[{
      pipelineBindPoint := .graphics,
      colorAttachments := #[{ attachment := 0, layout := .colorAttachmentOptimal }]
    }],
    dependencies := #[{
      srcSubpass := (0 : UInt32).complement,
      dstSubpass := 0,
      srcStageMask := .colorAttachmentOutput,
      dstStageMask := .colorAttachmentOutput,
      dstAccessMask := .colorAttachmentWrite,
    }]
  }

  let loadShaderModule (path : String) := do
    let code ← IO.FS.readBinFile path
    let (_, module) ← createShaderModule device { code }
    pure module

  let vertShaderModule ← loadShaderModule s!"{shaderPath}/shader.vert.spv"
  let fragShaderModule ← loadShaderModule s!"{shaderPath}/shader.frag.spv"

  let (_, pipelineLayout) ← createPipelineLayout device {}

  let (_, #[graphicsPipeline]) ← createGraphicsPipelines device #[{
    stages := #[
      { stage := .vertex, module := vertShaderModule, name := "main" },
      { stage := .fragment, module := fragShaderModule, name := "main" }
    ],
    vertexInputState := some {},
    inputAssemblyState := some { topology := .triangleList },
    viewportState := some {
      viewports := #[{
        x := 0,
        y := 0,
        width := extent.width.toNat.toFloat,
        height := extent.height.toNat.toFloat,
        minDepth := 0,
        maxDepth := 1,
      }],
      scissors := #[{
        offset := ⟨0, 0⟩,
        extent,
      }],
    },
    rasterizationState := some {
      polygonMode := .fill,
      lineWidth := 1,
      cullMode := .back,
      frontFace := .clockwise,
      depthBiasConstantFactor := 0,
      depthBiasClamp := 0,
      depthBiasSlopeFactor := 0,
    },
    multisampleState := some {
      rasterizationSamples := ._1,
      minSampleShading := 1,
    },
    colorBlendState := some {
      logicOp := .copy,
      attachments := #[{
        colorWriteMask := .r ||| .g ||| .b ||| .a,
        srcColorBlendFactor := .one,
        dstColorBlendFactor := .zero,
        colorBlendOp := .add,
        srcAlphaBlendFactor := .one,
        dstAlphaBlendFactor := .zero,
        alphaBlendOp := .add,
      }],
      blendConstants := [0, 0, 0, 0].toFloatArray,
    },
    dynamicState := some { dynamicStates := #[.viewport, .scissor] },
    layout := pipelineLayout,
    renderPass,
    subpass := 0,
  }]
    | error "createGraphicsPipelines length mismatch"

  destroyShaderModule device fragShaderModule
  destroyShaderModule device vertShaderModule

  let swapChainFramebuffers ← swapChainImageViews.mapM (λ imageView => do
    let (_, framebuffer) ← createFramebuffer device {
      renderPass,
      attachments := #[imageView],
      width := extent.width,
      height := extent.height,
      layers := 1,
    }
    pure framebuffer
  )

  let (_, commandPool) ← createCommandPool device {
    flags := .resetCommandBuffer,
    queueFamilyIndex,
  }

  let (_, #[commandBuffer]) ← allocateCommandBuffers device {
    commandPool,
    level := .primary,
    commandBufferCount := 1,
  }
    | error "allocateCommandBuffers length mismatch"

  let (_, imageAvailableSemaphore) ← createSemaphore device {}
  let (_, renderFinishedSemaphore) ← createSemaphore device {}
  let (_, inFlightFence) ← createFence device { flags := .signaled }

  let UINT64_MAX := (0 : UInt64).complement
  Glfw.run window (do
    let _ ← waitForFences device #[inFlightFence] 1 UINT64_MAX
    let _ ← resetFences device #[inFlightFence]
    let (_, imageIndex) ← acquireNextImageKHR device swapchain UINT64_MAX imageAvailableSemaphore

    let _ ← resetCommandBuffer commandBuffer default
    let _ ← beginCommandBuffer commandBuffer {}
    let some framebuffer := swapChainFramebuffers.get? imageIndex.toNat | error "imageIndex oob"
    let _ ← cmdBeginRenderPass commandBuffer {
      renderPass,
      framebuffer,
      renderArea := { offset := ⟨0, 0⟩, extent },
      clearValues := #[ClearValue.color (.float32 [0, 0, 0, 1].toFloatArray)]
    } .inline
    cmdBindPipeline commandBuffer .graphics graphicsPipeline
    cmdSetViewport commandBuffer 0 #[{
      x := 0,
      y := 0,
      width := extent.width.toNat.toFloat,
      height := extent.height.toNat.toFloat,
      minDepth := 0,
      maxDepth := 1,
    }]
    cmdSetScissor commandBuffer 0 #[{ offset := ⟨0, 0⟩, extent }]
    cmdDraw commandBuffer 3 1 0 0
    cmdEndRenderPass commandBuffer
    let _ ← endCommandBuffer commandBuffer

    let _ ← queueSubmit queue #[{
      waitSemaphores := #[imageAvailableSemaphore],
      waitDstStageMask := #[.colorAttachmentOutput],
      commandBuffers := #[commandBuffer],
      signalSemaphores := #[renderFinishedSemaphore],
    }] inFlightFence

    let _ ← queuePresentKHR queue {
      waitSemaphores := #[renderFinishedSemaphore],
      swapchains := #[swapchain],
      imageIndices := #[imageIndex],
    }
  )

  let _ ← deviceWaitIdle device

  destroySemaphore device imageAvailableSemaphore
  destroySemaphore device renderFinishedSemaphore
  destroyFence device inFlightFence
  destroyCommandPool device commandPool
  swapChainFramebuffers.forM (destroyFramebuffer device)
  destroyPipeline device graphicsPipeline
  destroyPipelineLayout device pipelineLayout
  swapChainImageViews.forM (destroyImageView device)
  destroyRenderPass device renderPass
  destroySwapchainKHR device swapchain
  destroyDevice device
  destroySurfaceKHR inst surface
  destroyInstance inst
