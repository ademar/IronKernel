import { EventEmitter } from "node:events";
import { beforeEach, describe, expect, it, vi } from "vitest";

const spawnMock = vi.hoisted(() => vi.fn());

vi.mock("node:child_process", () => ({
  spawn: (...args: unknown[]) => spawnMock(...args)
}));

import { runProcess } from "../src/cli.js";

describe("runProcess AbortError settlement", () => {
  beforeEach(() => {
    spawnMock.mockReset();
  });

  it("settles on AbortError without waiting for close or the full timeout", async () => {
    const fakeChild = new EventEmitter() as EventEmitter & {
      stdout: EventEmitter;
      stderr: EventEmitter;
      kill: () => boolean;
    };
    fakeChild.stdout = new EventEmitter();
    fakeChild.stderr = new EventEmitter();
    fakeChild.kill = () => true;
    spawnMock.mockReturnValue(fakeChild);

    const timeoutMs = 5000;
    const started = Date.now();
    const resultPromise = runProcess(
      { command: "unused", prefixArgs: [], cwd: "/" },
      [],
      { timeoutMs, maxOutputBytes: 4096 }
    );

    const abortError = new Error("The operation was aborted");
    abortError.name = "AbortError";
    fakeChild.emit("error", abortError);

    const result = await resultPromise;
    expect(result.cancelled).toBe(true);
    expect(result.timedOut).toBe(false);
    expect(result.exitCode).toBeNull();
    expect(Date.now() - started).toBeLessThan(timeoutMs / 2);
  });
});
