{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.vulkan-docs = {
    url = "github:KhronosGroup/Vulkan-Docs?ref=v1.3.265";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, vulkan-docs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
        lean4 = pkgs.lean4.overrideAttrs (_: rec {
          version = "4.2.0-rc4";
          src = pkgs.fetchFromGitHub {
            owner = "leanprover";
            repo = "lean4";
            rev = "v${version}";
            hash = "sha256-RE3/HN0roRLarAlFGN9HWBpZfzomox2bYbqL/KsQ+/w=";
          };
          buildInputs = [ pkgs.pkgsStatic.gmp ];
        });
        shaders = pkgs.stdenv.mkDerivation {
          name = "shaders";
          src = ./src/Vk/shaders;
          nativeBuildInputs = [ pkgs.shaderc ];
          buildPhase = ''
            mkdir -p $out
            glslc shader.vert -o $out/shader.vert.spv
            glslc shader.frag -o $out/shader.frag.spv
          '';
        };
      in
      {
        packages = {
          inherit shaders;
          default =
            pkgs.stdenv.mkDerivation {
              name = "lean-vulkan";
              src = ./.;
              nativeBuildInputs = [ lean4 pkgs.makeWrapper ];
              buildInputs = with pkgs; [ vulkan-headers vulkan-loader glfw-wayland ];
              buildPhase = ''
                # Hack to not have the FFI targets in the first phase.
                head -n 17 $src/lakefile.lean > lakefile.lean
                lake build generate
                ./build/bin/generate
                cp $src/lakefile.lean lakefile.lean
                lake build
              '';
              installPhase = "mkdir -p $out/bin && cp build/bin/demo $out/bin";
              fixupPhase = ''
                # Hack to make a Vulkan program run on a non-NixOS distro.
                # patchelf --remove-rpath --set-interpreter /usr/lib/ld-linux-x86-64.so.2 $out/bin/demo

                # Hack on NixOS if system glibc version differs from the version in the pkgs
                wrapProgram $out/bin/demo --prefix XDG_DATA_DIRS : "${pkgs.mesa.drivers}"/share
              '';
              vk_xml_path = "${vulkan-docs}/xml/vk.xml";
              shader_path = "${shaders}";
            };
        };
      });
}
