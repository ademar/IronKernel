import { describe, expect, it } from "vitest";
import { parseDiagnostics } from "../src/diagnostics.js";

describe("parseDiagnostics", () => {
  it("parses a ranged Unix diagnostic", () => {
    const diagnostics = parseDiagnostics(
      "Script error: /work/demo.scm:2:3: Getting an unbound variable: 'missing'\n" +
        "  (missing 42)\n" +
        "  ^^^^^^^^^^^^\n"
    );

    expect(diagnostics).toEqual([
      {
        file: "/work/demo.scm",
        line: 1,
        column: 2,
        endColumn: 14,
        message: "Getting an unbound variable: 'missing'"
      }
    ]);
  });

  it("parses Windows drive letters from the right-hand position fields", () => {
    const [diagnostic] = parseDiagnostics(
      "Compile error: C:\\source\\demo.scm:10:4: Parse error: Expecting ')'\n"
    );

    expect(diagnostic?.file).toBe("C:\\source\\demo.scm");
    expect(diagnostic?.line).toBe(9);
    expect(diagnostic?.column).toBe(3);
    expect(diagnostic?.endColumn).toBe(4);
  });

  it("ignores unrelated output", () => {
    expect(parseDiagnostics("Hello,world!\n")).toEqual([]);
  });
});
