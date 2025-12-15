const std = @import("std");

// Although this function looks imperative, it does not perform the build
// directly and instead it mutates the build graph (`b`) that will be then
// executed by an external runner. The functions in `std.Build` implement a DSL
// for defining build steps and express dependencies between them, allowing the
// build runner to parallelize the build automatically (and the cache system to
// know when a step doesn't need to be re-run).
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const raylib_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
    });

    const raylib = raylib_dep.module("raylib"); // main raylib module
    const raygui = raylib_dep.module("raygui"); // raygui module
    const raylib_artifact = raylib_dep.artifact("raylib"); // raylib C library

    const exe = b.addExecutable(.{
        .name = "graphing_engine",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
            },
        }),
    });

    exe.linkLibrary(raylib_artifact);
    exe.root_module.addImport("raylib", raylib);
    exe.root_module.addImport("raygui", raygui);

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // Tests for main executable
    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });

    // Tests for equation module
    const equation_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/equation.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    equation_tests.root_module.addImport("raylib", raylib);

    const run_exe_tests = b.addRunArtifact(exe_tests);
    const run_equation_tests = b.addRunArtifact(equation_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_exe_tests.step);
    test_step.dependOn(&run_equation_tests.step);

    // macOS App Bundle
    const app_step = b.step("app", "Build macOS .app bundle");

    const app_name = "Graphing Engine";
    const bundle_id = "com.graphing.engine";

    // Create app bundle structure
    const app_dir = b.fmt("{s}.app", .{app_name});
    const contents_dir = b.fmt("{s}/Contents", .{app_dir});
    const macos_dir = b.fmt("{s}/MacOS", .{contents_dir});
    const resources_dir = b.fmt("{s}/Resources", .{contents_dir});

    // Create directories
    const mkdir_app = b.addSystemCommand(&[_][]const u8{"mkdir", "-p", macos_dir});
    const mkdir_resources = b.addSystemCommand(&[_][]const u8{"mkdir", "-p", resources_dir});

    // Copy executable
    const copy_exe = b.addSystemCommand(&[_][]const u8{
        "cp",
        b.getInstallPath(.bin, exe.name),
        b.fmt("{s}/{s}", .{macos_dir, app_name}),
    });
    copy_exe.step.dependOn(b.getInstallStep());
    copy_exe.step.dependOn(&mkdir_app.step);

    // Create Info.plist
    const plist_content = b.fmt(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        \\<plist version="1.0">
        \\<dict>
        \\    <key>CFBundleName</key>
        \\    <string>{s}</string>
        \\    <key>CFBundleDisplayName</key>
        \\    <string>{s}</string>
        \\    <key>CFBundleIdentifier</key>
        \\    <string>{s}</string>
        \\    <key>CFBundleVersion</key>
        \\    <string>1.0.0</string>
        \\    <key>CFBundlePackageType</key>
        \\    <string>APPL</string>
        \\    <key>CFBundleExecutable</key>
        \\    <string>{s}</string>
        \\    <key>LSMinimumSystemVersion</key>
        \\    <string>11.0</string>
        \\    <key>NSHighResolutionCapable</key>
        \\    <true/>
        \\</dict>
        \\</plist>
        \\
    , .{app_name, app_name, bundle_id, app_name});

    const write_plist = b.addWriteFile(b.fmt("{s}/Info.plist", .{contents_dir}), plist_content);
    write_plist.step.dependOn(&mkdir_app.step);

    app_step.dependOn(&copy_exe.step);
    app_step.dependOn(&write_plist.step);
    app_step.dependOn(&mkdir_resources.step);
}
