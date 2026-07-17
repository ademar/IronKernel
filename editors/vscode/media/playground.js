(() => {
  const vscode = acquireVsCodeApi();
  const source = document.getElementById("source");
  const sample = document.getElementById("sample");
  const stdout = document.getElementById("stdout");
  const stderr = document.getElementById("stderr");
  const status = document.getElementById("status");
  const run = document.getElementById("run");
  const compile = document.getElementById("compile");
  const cancel = document.getElementById("cancel");
  const open = document.getElementById("open");

  const samples = {
    unless: `(define unless
  (vau (test & body) env
    (if (eval env test)
      #inert
      (eval env (cons sequence body)))))

(unless #f
  (. System.Console WriteLine "vau chooses when this runs"))`,
    environment: `(define scope
  (bindings->environment
    (message "Hello from a first-class environment")
    (say (lambda (x) (. System.Console WriteLine x)))))

(remote-eval (say message) scope)`,
    continuation: `(defn (yield x)
  (shift (lambda (k) (cons x (k (#inert))))))

(. System.Console WriteLine
  (reset (begin (yield 1) (yield 2) (yield 3) ())))`
  };

  function setRunning(value) {
    run.disabled = value;
    compile.disabled = value;
    cancel.disabled = !value;
    sample.disabled = value;
  }

  function loadSample() {
    source.value = samples[sample.value] || samples.unless;
    stdout.textContent = "";
    stderr.textContent = "";
    status.textContent = "Ready";
    vscode.setState({ sample: sample.value, source: source.value });
  }

  function execute(type) {
    stdout.textContent = "";
    stderr.textContent = "";
    vscode.postMessage({ version: 1, type, source: source.value });
  }

  sample.addEventListener("change", loadSample);
  source.addEventListener("input", () => {
    vscode.setState({ sample: sample.value, source: source.value });
  });
  run.addEventListener("click", () => execute("run"));
  compile.addEventListener("click", () => execute("compile"));
  cancel.addEventListener("click", () => {
    vscode.postMessage({ version: 1, type: "cancel" });
  });
  open.addEventListener("click", () => {
    vscode.postMessage({ version: 1, type: "open", source: source.value });
  });

  window.addEventListener("message", (event) => {
    const message = event.data;
    if (!message || typeof message.type !== "string") {
      return;
    }
    if (message.type === "started") {
      setRunning(true);
      status.textContent = message.mode === "compile" ? "Validating…" : "Running…";
      return;
    }
    if (message.type === "completed") {
      setRunning(false);
      stdout.textContent = message.stdout || "";
      stderr.textContent = message.stderr || "";
      if (message.cancelled) {
        status.textContent = "Cancelled";
      } else if (message.timedOut) {
        status.textContent = "Timed out";
      } else {
        status.textContent = message.exitCode === 0 ? "Complete" : `Exit ${message.exitCode}`;
      }
      return;
    }
    if (message.type === "failed") {
      setRunning(false);
      stderr.textContent = message.message || "Unable to run IronKernel.";
      status.textContent = "Failed";
    }
  });

  const previous = vscode.getState();
  if (previous && typeof previous.source === "string") {
    sample.value = previous.sample || "unless";
    source.value = previous.source;
  } else {
    loadSample();
  }
})();
