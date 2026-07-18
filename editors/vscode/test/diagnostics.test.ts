import { describe, expect, it } from "vitest";
import { parseDiagnostics } from "../src/diagnostics.js";

describe("parseDiagnostics", () => {
  it("parses a ranged Unix diagnostic", () => {
    const diagnostics = parseDiagnostics(
      "Script error: /work/demo.ikr:2:3: Getting an unbound variable: 'missing'\n" +
        "  (missing 42)\n" +
        "  ^^^^^^^^^^^^\n"
    );

    expect(diagnostics).toEqual([
      {
        file: "/work/demo.ikr",
        line: 1,
        column: 2,
        endColumn: 14,
        message: "Getting an unbound variable: 'missing'"
      }
    ]);
  });

  it("parses Windows drive letters from the right-hand position fields", () => {
    const [diagnostic] = parseDiagnostics(
      "Compile error: C:\\source\\demo.ikr:10:4: Parse error: Expecting ')'\n"
    );

    expect(diagnostic?.file).toBe("C:\\source\\demo.ikr");
    expect(diagnostic?.line).toBe(9);
    expect(diagnostic?.column).toBe(3);
    expect(diagnostic?.endColumn).toBe(4);
  });

  it("parses Project error diagnostics", () => {
    const [diagnostic] = parseDiagnostics(
      "Project error: /work/app/src/main.ikr:1:1: Getting an unbound variable: 'x'\n"
    );
    expect(diagnostic?.file).toBe("/work/app/src/main.ikr");
    expect(diagnostic?.message).toBe("Getting an unbound variable: 'x'");
  });

  it("ignores unrelated output", () => {
    expect(parseDiagnostics("Hello,world!\n")).toEqual([]);
  });
});
