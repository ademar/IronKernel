import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { describe, expect, it } from "vitest";
import { findIkprojWalkingUp, isIkprojPath, rankIkProjects } from "../src/projects.js";

describe("ikproj discovery", () => {
  it("recognizes .ikproj paths case-insensitively", () => {
    expect(isIkprojPath("/tmp/app.ikproj")).toBe(true);
    expect(isIkprojPath("/tmp/app.IKPROJ")).toBe(true);
    expect(isIkprojPath("/tmp/app.ikr")).toBe(false);
  });

  it("finds the nearest ancestor project while walking up", async () => {
    const root = await fs.mkdtemp(path.join(os.tmpdir(), "ikproj-"));
    const projectDir = path.join(root, "lantern");
    const srcDir = path.join(projectDir, "src");
    await fs.mkdir(srcDir, { recursive: true });
    const projectPath = path.join(projectDir, "lantern.ikproj");
    await fs.writeFile(projectPath, "<Project />\n");
    const sourcePath = path.join(srcDir, "main.ikr");
    await fs.writeFile(sourcePath, "#inert\n");

    await expect(findIkprojWalkingUp(sourcePath)).resolves.toBe(projectPath);
  });

  it("ranks projects that contain the active file first", () => {
    const ranked = rankIkProjects("/repo/Examples/lantern/src/main.ikr", [
      "/repo/other/app.ikproj",
      "/repo/Examples/lantern/lantern.ikproj"
    ]);
    expect(ranked[0]).toContain(`${path.sep}lantern${path.sep}lantern.ikproj`);
  });
});
