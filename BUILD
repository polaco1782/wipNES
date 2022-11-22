cc_library(
    name = "wipneslib",
    srcs = glob(["src/*.c"]),
    hdrs = glob(["include/*.h"]),
    strip_include_prefix = "include",
)

cc_binary(
    name = "wipnes",
    srcs = ["main.c"],
    deps = [":wipneslib"],
)