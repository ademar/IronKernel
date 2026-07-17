import * as path from "node:path";
import * as vscode from "vscode";
import { resolveRuntime, runProcess } from "./cli.js";
import { parseDiagnostics } from "./diagnostics.js";
import { PlaygroundPanel } from "./playgroundPanel.js";

const timeoutMs = 120000;

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel("IronKernel");
  const diagnostics = vscode.languages.createDiagnosticCollection("ironkernel");
  context.subscriptions.push(output, diagnostics);

  context.subscriptions.push(
    vscode.commands.registerCommand("ironkernel.showOutput", () => output.show()),
    vscode.commands.registerCommand("ironkernel.openPlayground", () => {
      PlaygroundPanel.createOrShow(context, output);
    }),
    vscode.commands.registerCommand("ironkernel.runFile", async () => {
      const document = await activeIronKernelDocument();
      if (document) {
        const args = vscode.workspace
          .getConfiguration("ironkernel", document.uri)
          .get<string[]>("runArgs", []);
        await executeDocument(document, ["run", document.uri.fsPath, ...args], output, diagnostics);
      }
    }),
    vscode.commands.registerCommand("ironkernel.compileFile", async () => {
      const document = await activeIronKernelDocument();
      if (document) {
        const parsed = path.parse(document.uri.fsPath);
        const outputPath = path.join(parsed.dir, `${parsed.name}.ikc`);
        await executeDocument(
          document,
          ["compile", document.uri.fsPath, "-o", outputPath],
          output,
          diagnostics
        );
      }
    })
  );
}

async function activeIronKernelDocument(): Promise<vscode.TextDocument | undefined> {
  if (!vscode.workspace.isTrusted) {
    void vscode.window.showWarningMessage(
      "Trust this workspace before executing IronKernel code."
    );
    return undefined;
  }

  const document = vscode.window.activeTextEditor?.document;
  if (!document || document.languageId !== "ironkernel") {
    void vscode.window.showInformationMessage("Open an IronKernel source file first.");
    return undefined;
  }
  if (document.isUntitled) {
    void vscode.window.showInformationMessage("Save the IronKernel file before running it.");
    return undefined;
  }
  if (document.isDirty && !(await document.save())) {
    return undefined;
  }
  return document;
}

async function executeDocument(
  document: vscode.TextDocument,
  args: string[],
  output: vscode.OutputChannel,
  collection: vscode.DiagnosticCollection
): Promise<void> {
  const folder = vscode.workspace.getWorkspaceFolder(document.uri);
  const configuration = vscode.workspace.getConfiguration("ironkernel", document.uri);
  let runtime;
  try {
    runtime = await resolveRuntime(
      folder?.uri.fsPath,
      configuration.get<string>("executablePath", ""),
      configuration.get<string>("projectPath", "IronKernel/IronKernel.fsproj")
    );
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    output.appendLine(`[launch failed] ${message}`);
    void vscode.window.showErrorMessage(`Unable to resolve IronKernel: ${message}`);
    return;
  }
  const maxOutputBytes = configuration.get<number>("maxOutputBytes", 1024 * 1024);
  collection.clear();
  output.show(true);
  output.appendLine(`> ${runtime.command} ${[...runtime.prefixArgs, ...args].join(" ")}`);

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: "Running IronKernel",
      cancellable: true
    },
    async (_progress, token) => {
      const controller = new AbortController();
      const cancellation = token.onCancellationRequested(() => controller.abort());
      try {
        const result = await runProcess(runtime, args, {
          timeoutMs,
          maxOutputBytes,
          signal: controller.signal
        });
        if (result.stdout) {
          output.append(result.stdout);
        }
        if (result.stderr) {
          output.append(result.stderr);
        }
        output.appendLine(
          `\n[exit ${result.exitCode ?? "unknown"}${result.cancelled ? ", cancelled" : ""}]`
        );
        publishDiagnostics(result.stderr, collection);
        if (result.exitCode !== 0 && !result.cancelled) {
          void vscode.window.showErrorMessage("IronKernel reported an error.");
        }
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        output.appendLine(`[launch failed] ${message}`);
        void vscode.window.showErrorMessage(`Unable to launch IronKernel: ${message}`);
      } finally {
        cancellation.dispose();
      }
    }
  );
}

function publishDiagnostics(
  stderr: string,
  collection: vscode.DiagnosticCollection
): void {
  const byDocument = new Map<string, vscode.Diagnostic[]>();
  for (const parsed of parseDiagnostics(stderr)) {
    const uri = vscode.Uri.file(parsed.file);
    const diagnostic = new vscode.Diagnostic(
      new vscode.Range(
        parsed.line,
        parsed.column,
        parsed.line,
        Math.max(parsed.column + 1, parsed.endColumn)
      ),
      parsed.message,
      vscode.DiagnosticSeverity.Error
    );
    diagnostic.source = "IronKernel";
    const key = uri.toString();
    const values = byDocument.get(key) ?? [];
    values.push(diagnostic);
    byDocument.set(key, values);
  }
  for (const [uri, values] of byDocument) {
    collection.set(vscode.Uri.parse(uri), values);
  }
}

export function deactivate(): void {
  // Resources registered in ExtensionContext are disposed by VS Code.
}
