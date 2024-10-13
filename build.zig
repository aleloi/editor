const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "editor",
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path("src/rope.zig"),
        .target = target,
        .optimize = optimize,
        //.emit_docs = .emit,
    });

    const docsget = b.addInstallDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    b.default_step.dependOn(&docsget.step);


    // TRACY
    const tracy_enable = b.option(bool, "tracy_enable", "Enable profiling") orelse true;
    const opts = b.addOptions();
    opts.addOption(bool, "enable_tracy", tracy_enable);

    const tracy = b.dependency("tracy", .{
        .target = target,
        .optimize = optimize,
        .tracy_enable = tracy_enable,
    });




    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "editor",
        .root_source_file = b.path("src/mini.zig"),
        .target = target,
        .optimize = optimize,
    });


    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const rope_mod = b.createModule(.{
        .root_source_file = b.path("src/rope.zig"),
        .imports = &.{
            //.{ .name = "zigrc", .module=zigrc_mod}
        }
    });

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");

    // const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const test_files: []const [] const u8 = &.{"tests/rope_tests.zig",
                                               "src/rope.zig",
                                               "src/render_buffer.zig",
                                               "src/document.zig",
                                               };
    for (test_files) |test_file| {
        const test_set = b.addTest(.{
            .root_source_file = b.path(test_file),
            .target = target,
            .optimize = optimize,
        });
        test_set.root_module.addImport("rope", rope_mod);
        var run_test_set = b.addRunArtifact(test_set);
        run_test_set.has_side_effects = true;
        test_step.dependOn(&run_test_set.step);
    }


    const exe_check = b.addExecutable(.{
        .name = "foo",
        .root_source_file = b.path("src/mini.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_check.root_module.addImport("rope", rope_mod);

    // Any other code to define dependencies would
    // probably be here.
    // const zigrc_dep = b.dependency("zigrc", .{
    //     .target = target,
    //     .optimize = optimize
    // });

    //const zigrc_mod = &zigrc_dep.artifact("zig-rc").root_module;
    // const leaking_rc_mod = b.createModule(.{
    //     .root_source_file = b.path("src/leaking_rc.zig"),
    //     // .imports = &.{
    //     //     .{ .name = "zigrc", .module=zigrc_mod}
    //     // }
    // });

    // //zigrc_mod = leaking_rc_mod;
    // _ = leaking_rc_mod;


    //exe.root_module.addImport("zigrc", zigrc_mod);
    exe.root_module.addImport("rope", rope_mod);
    exe.root_module.addImport("tracy", tracy.module("tracy"));
    exe.linkLibrary(tracy.artifact("tracy"));
    exe.linkLibCpp();

    // These two lines you might want to copy
    // (make sure to rename 'exe_check')
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);
}
