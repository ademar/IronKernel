import { promises as fs } from "node:fs";
import * as path from "node:path";
import { beforeAll, describe, expect, it } from "vitest";
import { loadWASM, OnigScanner, OnigString } from "vscode-oniguruma";
import { INITIAL, Registry, parseRawGrammar, type IGrammar } from "vscode-textmate";

let grammar: IGrammar;

beforeAll(async () => {
  const wasmPath = require.resolve("vscode-oniguruma/release/onig.wasm");
  await loadWASM(await fs.readFile(wasmPath));
  const grammarPath = path.resolve(__dirname, "../syntaxes/ironkernel.tmLanguage.json");
  const registry = new Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (sources) => new OnigScanner(sources),
      createOnigString: (value) => new OnigString(value)
    }),
    loadGrammar: async (scopeName) => {
      if (scopeName !== "source.ironkernel") {
        return null;
      }
      return parseRawGrammar(await fs.readFile(grammarPath, "utf8"), grammarPath);
    }
  });
  grammar = (await registry.loadGrammar("source.ironkernel")) as IGrammar;
});

function scopesFor(line: string): string[][] {
  return grammar.tokenizeLine(line, INITIAL).tokens.map((token) => token.scopes);
}

describe("IronKernel TextMate grammar", () => {
  it("colors operatives distinctly from applicatives", () => {
    const scopes = scopesFor("(define add (lambda (x y) (+ x y)))").flat();

    expect(scopes).toContain("keyword.control.operative.ironkernel");
    expect(scopes).toContain("support.function.applicative.ironkernel");
  });

  it("recognizes operative forms, constants, vectors, and comments", () => {
    const scopes = scopesFor("(vau (x & rest) env [#t :tag 42]) ; raw operands").flat();

    expect(scopes).toContain("keyword.control.operative.ironkernel");
    expect(scopes).toContain("keyword.operator.improper-list.ironkernel");
    expect(scopes).toContain("constant.language.ironkernel");
    expect(scopes).toContain("constant.other.keyword.ironkernel");
    expect(scopes).toContain("constant.numeric.ironkernel");
    expect(scopes).toContain("comment.line.semicolon.ironkernel");
  });

  it("recognizes Unicode aliases and CLR interop as operatives", () => {
    const scopes = scopesFor("(λ (x) (.get System.DateTime Now))").flat();

    expect(scopes).toContain("keyword.control.operative.ironkernel");
    expect(scopes.filter((scope) => scope.includes("operative")).length).toBeGreaterThanOrEqual(2);
  });

  it("does not treat vector as a prefix of vector-ref", () => {
    const line = "(vector-ref v 0)";
    const tokens = grammar.tokenizeLine(line, INITIAL).tokens;
    const vectorRef = tokens.find((token) => {
      const text = line.slice(token.startIndex, token.endIndex);
      return text === "vector-ref";
    });
    expect(vectorRef?.scopes).toContain("support.function.applicative.ironkernel");
  });
});
