import { randomBytes } from "node:crypto";
import * as vscode from "vscode";
import { executeSource, resolveRuntime } from "./cli.js";
import { parseDiagnostics } from "./diagnostics.js";

interface PlaygroundMessage {
  version: 1;
  type: "run" | "compile" | "cancel" | "open";
  source?: unknown;
}

const maximumSourceLength = 256 * 1024;

export class PlaygroundPanel implements vscode.Disposable {
  private static current: PlaygroundPanel | undefined;
  private readonly panel: vscode.WebviewPanel;
  private readonly disposables: vscode.Disposable[] = [];
  private activeRun: AbortController | undefined;
  private disposed = false;

  static createOrShow(
    context: vscode.ExtensionContext,
    output: vscode.OutputChannel
  ): PlaygroundPanel {
    if (PlaygroundPanel.current) {
      PlaygroundPanel.current.panel.reveal(vscode.ViewColumn.Beside);
      return PlaygroundPanel.current;
    }
    PlaygroundPanel.current = new PlaygroundPanel(context, output);
    return PlaygroundPanel.current;
  }

  private constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly output: vscode.OutputChannel
  ) {
    const mediaRoot = vscode.Uri.joinPath(context.extensionUri, "media");
    this.panel = vscode.window.createWebviewPanel(
      "ironkernelPlayground",
      "IronKernel Playground",
      vscode.ViewColumn.Beside,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
        localResourceRoots: [mediaRoot]
      }
    );
    this.panel.webview.html = this.html(mediaRoot);
    this.disposables.push(
      this.panel.onDidDispose(() => this.dispose()),
      this.panel.webview.onDidReceiveMessage((message: unknown) => this.receive(message))
    );
  }

  private async receive(rawMessage: unknown): Promise<void> {
    if (!this.isMessage(rawMessage)) {
      return;
    }

    if (rawMessage.type === "cancel") {
      this.activeRun?.abort();
      return;
    }

    if (rawMessage.type === "open") {
      if (typeof rawMessage.source !== "string" || rawMessage.source.length > maximumSourceLength) {
        return;
      }
      const document = await vscode.workspace.openTextDocument({
        language: "ironkernel",
        content: rawMessage.source
      });
      await vscode.window.showTextDocument(document);
      return;
    }

    if (!vscode.workspace.isTrusted) {
      await this.panel.webview.postMessage({
        type: "failed",
        message: "Trust this workspace before executing IronKernel code."
      });
      return;
    }
    if (typeof rawMessage.source !== "string" || rawMessage.source.length > maximumSourceLength) {
      await this.panel.webview.postMessage({
        type: "failed",
        message: "Playground source is missing or too large."
      });
      return;
    }

    this.activeRun?.abort();
    const controller = new AbortController();
    this.activeRun = controller;
    await this.panel.webview.postMessage({ type: "started", mode: rawMessage.type });

    try {
      const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
      const configuration = vscode.workspace.getConfiguration("ironkernel");
      const runtime = await resolveRuntime(
        workspaceFolder,
        configuration.get<string>("executablePath", ""),
        configuration.get<string>("projectPath", "IronKernel/IronKernel.fsproj")
      );
      const timeoutMs = configuration.get<number>("playground.timeoutMs", 10000);
      const maxOutputBytes = configuration.get<number>("maxOutputBytes", 1024 * 1024);

      this.output.appendLine(
        `[playground] ${runtime.command} ${[...runtime.prefixArgs, rawMessage.type].join(" ")}`
      );
      const result = await executeSource(
        runtime,
        rawMessage.source,
        rawMessage.type,
        [],
        { timeoutMs, maxOutputBytes, signal: controller.signal }
      );
      if (this.activeRun !== controller) {
        return;
      }
      await this.panel.webview.postMessage({
        type: "completed",
        ...result,
        diagnostics: parseDiagnostics(result.stderr)
      });
    } catch (error) {
      if (this.activeRun !== controller) {
        return;
      }
      const message = error instanceof Error ? error.message : String(error);
      this.output.appendLine(`[playground] ${message}`);
      await this.panel.webview.postMessage({ type: "failed", message });
    } finally {
      if (this.activeRun === controller) {
        this.activeRun = undefined;
      }
    }
  }

  private isMessage(value: unknown): value is PlaygroundMessage {
    if (typeof value !== "object" || value === null) {
      return false;
    }
    const candidate = value as Record<string, unknown>;
    const keys = Object.keys(candidate);
    return (
      keys.every((key) => ["version", "type", "source"].includes(key)) &&
      candidate.version === 1 &&
      typeof candidate.type === "string" &&
      ["run", "compile", "cancel", "open"].includes(candidate.type)
    );
  }

  private html(mediaRoot: vscode.Uri): string {
    const webview = this.panel.webview;
    const nonce = randomBytes(18).toString("base64");
    const styleUri = webview.asWebviewUri(vscode.Uri.joinPath(mediaRoot, "playground.css"));
    const scriptUri = webview.asWebviewUri(vscode.Uri.joinPath(mediaRoot, "playground.js"));
    return `<!doctype html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy"
        content="default-src 'none'; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';">
  <link rel="stylesheet" href="${styleUri}">
  <title>IronKernel Playground</title>
</head>
<body>
  <header>
    <div>
      <p class="eyebrow">REAL RUNTIME</p>
      <h1>IronKernel Playground</h1>
      <p>Explore operatives with the same CLI used by scripts and packages.</p>
    </div>
    <div class="actions">
      <button id="run" class="primary">Run</button>
      <button id="compile">Validate</button>
      <button id="cancel" disabled>Cancel</button>
      <button id="open">Open in editor</button>
    </div>
  </header>
  <main>
    <section class="editor-pane">
      <label for="sample">Example</label>
      <select id="sample">
        <option value="unless">Build unless with vau</option>
        <option value="environment">First-class environment</option>
        <option value="continuation">Delimited continuation</option>
      </select>
      <label for="source">Source</label>
      <textarea id="source" spellcheck="false" aria-label="IronKernel source"></textarea>
    </section>
    <section class="output-pane" aria-live="polite">
      <div class="status-row">
        <h2>Output</h2>
        <span id="status">Ready</span>
      </div>
      <pre id="stdout"></pre>
      <pre id="stderr" class="error"></pre>
    </section>
  </main>
  <script nonce="${nonce}" src="${scriptUri}"></script>
</body>
</html>`;
  }

  dispose(): void {
    if (this.disposed) {
      return;
    }
    this.disposed = true;
    this.activeRun?.abort();
    this.activeRun = undefined;
    if (PlaygroundPanel.current === this) {
      PlaygroundPanel.current = undefined;
    }
    while (this.disposables.length > 0) {
      this.disposables.pop()?.dispose();
    }
    this.panel.dispose();
  }
}
