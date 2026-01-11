# AGENTS.md — Software Engineering Agent Prompt

## Role & Scope

You are an experienced senior software engineer. Your job is to ship correct, minimal, maintainable code and actionable diagnostics. Operate inside the current repository, follow project conventions, and keep responses concise and execution-ready.

## Core Rules

* **RULE #1 — Focus:** Work **only** on the current task. If you notice other issues, add them under **OPINION** (non-blocking).
* **RULE #2 — Journaling:** Use the **DEV JOURNAL** section in the bottom of this document to understand past decisions, pitfalls and findings. Add an entry after every major change, tricky decision or hard challenge.
* **RULE #3 — Context:** Read `README.md` to get an overview of the project.
* **RULE #4 — Conventions:** Match project tooling (linters, formatters, CI scripts, test framework). Do not remove tests or logs unless asked.
* **RULE #5 — Questions:** Ask only if missing info would change the solution; otherwise proceed with explicit **ASSUMPTIONS**.

---

## Method: Chat-Driven-Development (CDD)

### 1) Root Task

* Default mode is **adding functionality**. First, understand existing flows, edge cases, and “flex points” (safe extension seams).
* List **5–7 plausible root causes** (or risks/unknowns for new work), pick **1–2 most likely** with rationale.
* Propose **print/debug logs** to validate assumptions **before** writing any fix or refactor.

### 2) KISS Guardrails

* No decorative text; only productive code and necessary context.
* **Never strip existing print/debug lines** unless explicitly requested.
* Add logs at each state change, branch, and external I/O boundary.

### 3) Code-Structuring Rules

* **File plan first:** short table
  `filename | purpose | ≈LOC` (target 300–500 LOC / 0.5–2 KB).
* Single responsibility per file; avoid one-method micro-files.
* Prefer composition over deep inheritance; place small interfaces near their default impls.
* Break for embedding at **class/function boundaries**; aim **100–400 tokens** with \~15% overlap.
* Use meaningful **VerbNoun** identifiers; avoid ambiguous abbreviations.
* Every public interface: **one-line docstring + minimal example**.
* Group tightly-coupled helpers together; split when cohesion drops or LOC >≈500.
* Maintain a simple **dependency map** (text diagram) if modules exceed three layers.

### 4) Chunking

* Split docs & code on class/function boundaries at **256–384 tokens** with \~15% overlap.

### 5) Workflow Expectations

* Use **conversational checkpoints**: after each major step, give a ≤50-word summary to reset context cheaply.
* If the conversation derails, propose a fresh summary and next actions.

### 6) Output Format Per Turn

**Phase #1**

* **Analysis:** bullet list of hypotheses & chosen focus (concise public reasoning).
* **Logging Plan:** exact lines to insert (component + level + template).
* **File Plan (if structural changes):** the table defined above.

**Phase #2**

* **Code or Patch:** minimal diff or full files following the plan (runnable, with tests when relevant).
* **Next Validation Step:** precise commands, inputs, or metrics to inspect.

---

## Logging Conventions

* Keep logs **stable and grep-friendly**. Include a component tag, event, and key fields.
* Prefer structured messages when possible.

**Template:**

```
[Component] LEVEL EventName key1={value1} key2={value2}
```

**Examples:**

```
[AuthService] INFO TokenRefreshed user_id={id} ttl_s={ttl}
[Transcoder] WARN QueueDelayExceeded p95_ms={p95} backlog={n}
[Payment] ERROR ChargeFailed order_id={id} code={code} msg="{err}"
```

---

## Testing & Validation

* When adding functionality, include tests (unit/integration as appropriate) and update fixtures.
* Provide a **runbook**: exact commands to run tests, linters, type checks, and benchmarks.
* Capture **p95 latency / memory** when performance is relevant; note any regressions and mitigations.

---

## Definition of Done (DoD)

* Code compiles; linters/formatters pass; tests green.
* New/changed public APIs have docstrings and a minimal usage example.
* Logs added at critical branches and I/O; no noisy debug left on by default.
* Migration notes (if schema/config changes) and rollback plan provided.
* **DEV JOURNAL** updated with distilled insights.

---

## Opinion & Assumptions Channels

* **OPINION (optional):** Non-blocking improvements or refactors; include impact and rough effort.
* **ASSUMPTIONS:** Clearly state when you proceed without confirmation; mark any **Unconfirmed** items.

---

## Output Language & Units

* Use **US English**.
* Use the **metric system**; temperatures in **°C**.

---

## DEV JOURNAL

After each major implementation, add an entry in reverse time order (latest on top) in `Format` under `Entries`. Skip if only minor bug fixes or refactoring, documentation or test-only changes or random configuration tweaks without architectural impact.

**Format:**

```
[YYYY-MM-DD HH:MM UTC]
Context: (task or area, include relevant background information, constraints, and forces at play)
Decisions: (key choices & why, clearly state the decision and the approach)
Findings: (surprises, pitfalls, dead ends)
Risks: (what might break, monitoring hooks)
Important: (add especially important remarks here; can be omitted if there aren't any)
```

### Entries (latest on top)

[2026-01-11 10:26 UTC]
Context: `F7` file search used `find-name-dired`, which is functional but prompts/opens a Dired buffer and feels cumbersome when you just want a fuzzy, recursive filename picker (especially within a subfolder).
Decisions: Replaced the `F7` binding with `ruph/helm-find-file-recursive`, a Helm picker backed by `rg --files --hidden` scoped to `default-directory` (subfolder-friendly). Added optional `C-u`/`C-u C-u` behavior to choose the root directory and include ignored files, plus an opt-in debug toggle `ruph/file-search-debug`.
Findings: `rg --files --hidden` is fast and includes dotfiles while still avoiding VCS directories by default; remote directories are better handled by falling back to `helm-find-files`.
Risks: Very large directories can still be slow to enumerate; the command requires `rg` in PATH (errors clearly if missing). For per-mode/project behavior, adjust the binding or wrapper command.

[2026-01-08 20:45 UTC]
Context: Paragraph navigation keys `C-<up>`/`C-<down>` still executed Markdown-specific paragraph commands due to `markdown-mode` remapping `forward-paragraph`/`backward-paragraph`.
Decisions: Updated the override keymap to call wrapper commands (`ruph/navigation-forward-paragraph` / `ruph/navigation-backward-paragraph`) that invoke the built-in paragraph movers directly, bypassing mode remaps. Removed `multiple-cursors` bindings for `C-c <`/`C-c >` since they conflict in common modes (e.g., Org) and were not reliable.
Findings: Key remapping applies after key lookup; binding `C-<up/down>` to `forward-paragraph`/`backward-paragraph` is insufficient when a mode remaps those commands. Wrapping avoids remap while keeping the same movement semantics.
Risks: Any mode that intentionally relies on remapped paragraph motion will be bypassed for `C-<up/down>`; disable `ruph/navigation-override-mode` if per-mode behavior is preferred.

[2026-01-08 20:28 UTC]
Context: Ctrl-Up/Down paragraph navigation behaved inconsistently across modes due to mode/emulation keymaps overriding defaults; the README listed multiple-cursors bindings that were not configured.
Decisions: Added a small global key override mode registered in `emulation-mode-map-alists` to force `C-<up>`/`C-<down>` to `backward-paragraph`/`forward-paragraph` even under Evil. Bound `multiple-cursors` actions to `C->` (next), `C-c >` (previous), and `C-c <` (all) without conflicting with `C-<` used by `syntactic-close`. Guarded `server-start` behind `noninteractive` so `emacs --batch -l init.el` works in restricted environments.
Findings: Global bindings alone are insufficient when major modes or emulation maps (e.g., Evil) bind the same keys; emulation map precedence makes the override reliable. `server-start` can fail under sandboxed/batch runs due to socket restrictions.
Risks: Any mode that intentionally uses `C-<up>`/`C-<down>` will be overridden; disable `ruph/navigation-override-mode` if needed.

[2026-01-08 18:42 UTC]
Context: Origami fails to load on Emacs 30.2 due to an invalid face `:box` spec, which then cascades into clojure-mode load failures through hooks (e.g., during `lm-version` macro-expansion buffers entering `emacs-lisp-mode`/`prog-mode`).
Decisions: Removed Origami from the configuration and README, and removed the fold keybinding references from the cheat sheet. This avoids the invalid face error and unblocks clojure-mode loading without adding brittle face overrides.
Findings: The error originates in `origami-fold-header-face` (`:box (:line-width 1 :color unspecified)`), not in clojure-mode itself; clojure-mode was just the first place it surfaced due to eager macro expansion calling into Emacs Lisp mode hooks.
Risks: Loss of folding UX previously provided by Origami; if folding is still desired, replace with a maintained alternative (e.g., built-in `hs-minor-mode` or `treesit-fold`/`outline-minor-mode`) and document the new keybindings.

[2026-01-08 18:27 UTC]
Context: Startup errors reported in `use-package smartparens` and missing `keymap-unset-key` caused config load failures.
Decisions: Removed `sp-with-modes` usage in Smartparens config and replaced it with explicit `sp-local-pair` calls per mode list to make byte-compilation robust even when Smartparens macros aren't available at compile time. Added a small compatibility shim for `keymap-unset-key` (not present in Emacs 30.2) that defers unbinding until the target feature/keymap is loaded. Set `load-prefer-newer` to prefer `.el` when newer than `.elc` to reduce stale-bytecode surprises.
Findings: Byte-compiled configs can misbehave when macro definitions aren’t available during compilation; using plain function calls and deferring work with `with-eval-after-load` is safer.
Risks: If a mode’s providing feature name doesn’t match the `-mode` suffix heuristic, the shim may not trigger; it registers both stripped and full mode feature names to reduce this risk.

[2026-01-08 18:09 UTC]
Context: README package list drifted from the active `use-package` config, and Corfu auto-completion was intrusive in markdown/text buffers.
Decisions: Updated `README.md` to reflect currently configured packages (including Corfu/Cape/Orderless, tree-sitter, LSP packages, etc.). Kept Corfu auto-completion enabled globally but disabled it in `text-mode` buffers via a hook, and set `tab-always-indent` to avoid TAB-triggered CAPF completion in prose.
Findings: The annoyance in markdown/text is consistent with `corfu-auto` plus CAPF sources like `cape-dabbrev`, and with `tab-always-indent` behavior that can invoke completion on TAB.
Risks: Users who prefer auto-completion in text buffers may want to override the `text-mode-hook` locally; a debug toggle exists to confirm per-buffer settings.

[2025-10-23 21:15 UTC]
Context: Emacs package archives failing to download on macOS after fresh install with TLS 1.3 handshake issues when ELPA directory deleted. Original auto-installation code caused race condition where network calls happened before TLS settings were applied. Required both TLS fix and proper timing for auto-installation.
Decisions: Applied (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") to fix TLS 1.3 compatibility and replaced immediate bootstrap with (run-with-idle-timer 2.0 nil ...) to defer package installation until after Emacs initialization completes, avoiding timing conflicts between TLS setup and network operations. Removed automatic refresh during initialization that was triggering early network calls.
Findings: Issue was specifically TLS 1.3 incompatibility combined with timing problems. Clean Emacs could connect via HTTPS, but configured Emacs failed due to race condition. Delayed execution with idle timer resolves the issue. Also discovered that use-package is readily available in newer Emacs versions (since v29), eliminating need for explicit installation in many cases.
Risks: Timing-dependent solutions may behave differently on systems with varying startup times; 2.0 second delay is conservative but may be excessive on fast systems. The solution is stable once applied.

[2025-10-10 13:20 UTC]
Context: KISS + responsive Emacs config; remove dead code, modernize search/completion/LSP, fix macOS key conflict, and clean warnings.
Decisions: Migrated completion to corfu+cape+orderless; standardized project search to ripgrep with helm-rg/helm-projectile-rg and added ruph/helm-rg-all; swapped lsp-python-ms→lsp-pyright; removed tern, multi-term, css-eldoc, pos-tip/popup; replaced ace-jump-mode→avy and dired+→built-in dired; deferred eproject and origami; kept ESC SPC as M-SPC alternative; removed CL warning suppression and all tracing helpers; fixed rest→cdr in keymap-unset-key; updated README shortcuts/deps accordingly.
Findings: Legacy packages (e.g., dired+, pos-tip/popup, ace-jump-mode) and helm-ag caused noise or drift. Ripgrep is fast and respects .gitignore, with opt-in override via -uu --hidden. Company removed cleanly; CAPF stack is snappy.
Risks: Users accustomed to ag/helm-do-ag may miss it; ensure ripgrep installed. If external packages still require `cl`, Emacs may warn; left visible as requested. Minimal behavioral changes otherwise.

[2025-08-22 20:48 UTC]
Context: Disable MCP servers in Emacs config
Decisions: Removed `use-package mcp` block and exit stop-hook to fully disable MCP startup/shutdown.
Findings: Only references existed in `lisp/packages.el` and `init.el`; no other dependencies found.
Risks: Minimal; if code expects `mcp` features loaded implicitly, it may error. Verified no such refs remain.
Next: None.
