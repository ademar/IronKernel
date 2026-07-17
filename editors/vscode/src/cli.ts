import { spawn } from "node:child_process";
import { promises as fs } from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

export interface RuntimeCommand {
  command: string;
  prefixArgs: string[];
  cwd: string;
}

export interface ProcessResult {
  exitCode: number | null;
  stdout: string;
  stderr: string;
  timedOut: boolean;
  cancelled: boolean;
}

export interface RunOptions {
  timeoutMs: number;
  maxOutputBytes: number;
  signal?: AbortSignal;
}

export async function resolveRuntime(
  workspaceDirectory: string | undefined,
  executablePath: string,
  projectPath: string
): Promise<RuntimeCommand> {
  if (executablePath.trim() !== "") {
    if (!path.isAbsolute(executablePath)) {
      throw new Error("ironkernel.executablePath must be an absolute path.");
    }
    await fs.access(executablePath);
    return {
      command: executablePath,
      prefixArgs: [],
      cwd: path.dirname(executablePath)
    };
  }

  if (workspaceDirectory) {
    const candidate = path.resolve(workspaceDirectory, projectPath);
    try {
      await fs.access(candidate);
      return {
        command: "dotnet",
        prefixArgs: ["run", "--project", candidate, "--"],
        cwd: workspaceDirectory
      };
    } catch {
      // Fall through to a release binary on PATH.
    }
  }

  return {
    command: process.platform === "win32" ? "IronKernel.exe" : "IronKernel",
    prefixArgs: [],
    cwd: workspaceDirectory ?? process.cwd()
  };
}

function appendCapped(
  chunks: Buffer[],
  chunk: Buffer,
  currentBytes: number,
  maximumBytes: number
): number {
  const remaining = maximumBytes - currentBytes;
  if (remaining <= 0) {
    return currentBytes;
  }
  chunks.push(chunk.subarray(0, remaining));
  return currentBytes + Math.min(chunk.length, remaining);
}

export function runProcess(
  runtime: RuntimeCommand,
  args: readonly string[],
  options: RunOptions
): Promise<ProcessResult> {
  return new Promise((resolve, reject) => {
    const stdout: Buffer[] = [];
    const stderr: Buffer[] = [];
    let stdoutBytes = 0;
    let stderrBytes = 0;
    let timedOut = false;
    let cancelled = false;
    let settled = false;

    const child = spawn(runtime.command, [...runtime.prefixArgs, ...args], {
      cwd: runtime.cwd,
      shell: false,
      windowsHide: true,
      signal: options.signal
    });

    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill();
    }, options.timeoutMs);

    child.stdout.on("data", (chunk: Buffer) => {
      stdoutBytes = appendCapped(stdout, chunk, stdoutBytes, options.maxOutputBytes);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      stderrBytes = appendCapped(stderr, chunk, stderrBytes, options.maxOutputBytes);
    });

    child.on("error", (error: NodeJS.ErrnoException) => {
      if (settled) {
        return;
      }
      if (error.name === "AbortError") {
        settled = true;
        clearTimeout(timeout);
        resolve({
          exitCode: null,
          stdout: Buffer.concat(stdout).toString("utf8"),
          stderr: Buffer.concat(stderr).toString("utf8"),
          timedOut,
          cancelled: true
        });
        return;
      }
      settled = true;
      clearTimeout(timeout);
      reject(error);
    });

    child.on("close", (exitCode) => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      cancelled = cancelled || options.signal?.aborted === true;
      resolve({
        exitCode,
        stdout: Buffer.concat(stdout).toString("utf8"),
        stderr: Buffer.concat(stderr).toString("utf8"),
        timedOut,
        cancelled
      });
    });
  });
}

export async function executeSource(
  runtime: RuntimeCommand,
  source: string,
  mode: "run" | "compile",
  scriptArgs: readonly string[],
  options: RunOptions
): Promise<ProcessResult> {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "ironkernel-playground-"));
  const sourcePath = path.join(directory, "playground.scm");
  const packagePath = path.join(directory, "playground.ikc");

  try {
    await fs.writeFile(sourcePath, source, "utf8");
    const args =
      mode === "compile"
        ? ["compile", sourcePath, "-o", packagePath]
        : ["run", sourcePath, ...scriptArgs];
    return await runProcess(runtime, args, options);
  } finally {
    await fs.rm(directory, { recursive: true, force: true });
  }
}
