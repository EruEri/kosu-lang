let kosu_target_arch = "arm64"
let kosu_target_os = "darwin"
let kosu_target_os_extentsion = ".dylib"
let kosu_target_cc = "cc"
let kosu_target_hash = "76e9c2a-dirty"
let kosu_target_branch = "install/makefile"
let kosu_target_headers = "/usr/local/opt/kosu/include/kosu"
let kosu_target_core_path = "/usr/local/opt/kosu/share/kosu/std"
let kosu_target_runtime_path = "/usr/local/opt/kosu/lib"

let kosu_target_linker_option =
  [ "syslibroot `xcrun --sdk macosx --show-sdk-path`"; "lSystem" ]

let kosu_target_linker_args = []
