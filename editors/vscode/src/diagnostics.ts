export interface ParsedDiagnostic {
  file: string;
  line: number;
  column: number;
  endColumn: number;
  message: string;
}

const headerPattern =
  /^(?:Script error|Compile error|Package error|Startup error):\s+(.+):(\d+):(\d+):\s+(.*)$/;

export function parseDiagnostics(stderr: string): ParsedDiagnostic[] {
  const lines = stderr.replace(/\r\n/g, "\n").split("\n");
  const diagnostics: ParsedDiagnostic[] = [];

  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index] ?? "";
    const match = headerPattern.exec(line);
    if (!match) {
      continue;
    }

    const lineNumber = Number.parseInt(match[2] ?? "1", 10);
    const columnNumber = Number.parseInt(match[3] ?? "1", 10);
    const caretLine = lines[index + 2] ?? "";
    const caretMatch = /^(\s*)(\^+)/.exec(caretLine);
    const caretWidth = caretMatch?.[2]?.length ?? 1;

    diagnostics.push({
      file: match[1] ?? "",
      line: Math.max(0, lineNumber - 1),
      column: Math.max(0, columnNumber - 1),
      endColumn: Math.max(0, columnNumber - 1) + caretWidth,
      message: (match[4] ?? "IronKernel error").trim()
    });
  }

  return diagnostics;
}
