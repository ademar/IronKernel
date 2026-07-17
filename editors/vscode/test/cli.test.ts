import * as path from "node:path";
import { describe, expect, it } from "vitest";
import { executeSource, resolveRuntime, runProcess } from "../src/cli.js";

const repositoryRoot = path.resolve(__dirname, "../../..");

describe("CLI process boundary", () => {
  it("passes arguments literally without a shell", async () => {
    const result = await runProcess(
      {
        command: process.execPath,
        prefixArgs: ["-e", "console.log(JSON.stringify(process.argv.slice(1)))", "--"],
        cwd: repositoryRoot
      },
      ["$(touch should-not-exist)", "a;echo injected"],
      { timeoutMs: 5000, maxOutputBytes: 4096 }
    );

    expect(result.exitCode).toBe(0);
    expect(JSON.parse(result.stdout)).toEqual([
      "$(touch should-not-exist)",
      "a;echo injected"
    ]);
  });

  it("rejects a relative executable override", async () => {
    await expect(resolveRuntime(repositoryRoot, "./IronKernel", "unused")).rejects.toThrow(
      "absolute path"
    );
  });

  it("caps output without invoking a shell", async () => {
    const result = await runProcess(
      {
        command: process.execPath,
        prefixArgs: ["-e", 'process.stdout.write("x".repeat(10000))'],
        cwd: repositoryRoot
      },
      [],
      { timeoutMs: 5000, maxOutputBytes: 64 }
    );

    expect(result.exitCode).toBe(0);
    expect(Buffer.byteLength(result.stdout)).toBe(64);
  });

  it("terminates a timed-out process", async () => {
    const result = await runProcess(
      {
        command: process.execPath,
        prefixArgs: ["-e", "setInterval(() => {}, 1000)"],
        cwd: repositoryRoot
      },
      [],
      { timeoutMs: 100, maxOutputBytes: 4096 }
    );

    expect(result.timedOut).toBe(true);
    expect(result.exitCode).not.toBe(0);
  });

  it(
    "runs playground source through the real IronKernel CLI",
    async () => {
      const runtime = await resolveRuntime(
        repositoryRoot,
        "",
        "IronKernel/IronKernel.fsproj"
      );
      const result = await executeSource(
        runtime,
        '(. System.Console WriteLine "playground-ok")',
        "run",
        [],
        { timeoutMs: 60000, maxOutputBytes: 64 * 1024 }
      );

      expect(result.exitCode).toBe(0);
      expect(result.stdout).toContain("playground-ok");
      expect(result.stderr).toBe("");
    },
    70000
  );
});
