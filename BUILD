cc_library(
    name = "wipneslib",
    srcs = glob(["src/*.c"]),
    hdrs = glob(["inc/*.h"]),
    #strip_include_prefix = "include",
)

cc_binary(
    name = "wipnes",
    srcs = ["main.c"],
    deps = [":wipneslib"],
    data = [
        ":wipnes_data"
    ]
)

# Copy test rom files to the output directory
filegroup(
    name = "wipnes_data",
    srcs = glob(["testroms/**"]),
)
