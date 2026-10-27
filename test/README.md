# Regression tests

The suite uses ERT with real nerd-icons, Projectile and Flymake libraries. There
is no shell runner or batch entry-point file. Every `*-tests.el` can be loaded
and run independently; `thattem-mode-line-test-helper.el` contains shared
fixtures, not substitute production implementations.

## Files

- `thattem-mode-line-mode-tests.el`: activation, restoration, hook timing,
  repeated cycles, empty configuration, buffer-local formats and keymap ownership.
- `thattem-mode-line-segments-tests.el`: segment variables, truncation boundaries,
  Unicode, modified/read-only states, unspecified coding and non-file buffers.
- `thattem-mode-line-faces-tests.el`: active/inactive and styled/fallback faces,
  buffer isolation and attribute nonmutation/replacement.
- `thattem-mode-line-macros-tests.el`: generated variant behavior, redefinition
  and mode/header alignment contracts, including an empty remaining format.
- `thattem-mode-line-directory-tests.el`: local/root/relative/TRAMP paths,
  full navigation targets, Unicode/space/percent names, both Dired directory
  representations, collapse boundaries and independent window scrolling.
- `thattem-mode-line-actions-tests.el`: event-window selection and error cleanup,
  both line areas, nonproject errors, menu ordering, empty menus and cancellation.
- `thattem-mode-line-flymake-tests.el`: real synchronous diagnostic reports,
  custom categories, cleared reports, narrowing preservation, width thresholds,
  filtered navigation, wrapping and missing diagnostics.

## Run all suites

From the repository root, with dependencies already on Emacs's load path:

```sh
emacs -Q --batch -L . -L test \
  --eval '(progn
    (dolist (file (directory-files "test" t "-tests[.]el$"))
      (load file nil t))
    (ert-run-tests-batch-and-exit "^thattem-mode-line-test--"))'
```

For separately checked-out dependencies, add `-L /path/to/nerd-icons.el`,
`-L /path/to/nerd-icons.el/data`, and `-L /path/to/projectile` before `--eval`.
The `data` directory is needed by some nerd-icons versions. `-Q` does not load
your init file or activate a package directory.

On Nix, enter an environment with the dependencies first:

```sh
nix-shell -p '(emacsPackagesFor emacs-nox).emacsWithPackages (ep: [ ep.nerd-icons ep.projectile ])'
```

Then run the Emacs command above. Use a pinned nixpkgs revision for reproducible
dependency versions. Verification here used Emacs 31.1, not an older-version
compatibility matrix.

## Run one suite or selected tests

```sh
emacs -Q --batch -L . -L test \
  -l test/thattem-mode-line-directory-tests.el \
  -f ert-run-tests-batch-and-exit
```

Alternatively, replace `-f ert-run-tests-batch-and-exit` with:

```sh
--eval '(ert-run-tests-batch-and-exit "thattem-mode-line-test--.*remote")'
```

Interactive use: add the project and `test` directories to `load-path`, load
one or more suites, then use `M-x ert`. Loading a suite does not run tests or
exit Emacs.

## Source and compiled verification

Do source testing in a clean copy without `.elc` files. For compiled testing,
copy the seven package files and `test/` to a temporary directory, then run the
following there, with dependency paths as needed:

```sh
emacs -Q --batch -L . -L test \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile thattem-mode-line*.el test/*.el
```

Run the all-suites command again in a fresh process, changing its file pattern
from `"-tests[.]el$"` to `"-tests[.]elc$"` to load compiled suites explicitly.
Verify the loaded package and helper paths in `load-history` when diagnosing
source fallback or an installed copy shadowing the checkout. Never compile in
the working source tree merely to run tests.

ERT returns a nonzero process exit on unexpected failures. Strict byte
compilation also fails on warnings. There is no automatic package installation,
network download or CI configuration in this test directory.

## Intent and coverage limits

Assertions check behavior and text-property contracts, not exact icon glyphs,
macro-expansion snapshots, incidental call counts or performance timings.
Whole-format replacement by overlapping unrelated modes has no defined
ownership protocol here; the suite does not invent one.

Mouse tests invoke the registered callback using constructed events and real
buffer/window fixtures. Popup and file-opening boundaries are intercepted to
avoid waiting for input or touching user files. The project-click regression
uses real Projectile root resolution. Flymake fixtures publish real diagnostics
through its report callback and mark only their in-memory backend safe on Emacs
versions enforcing trust. TRAMP tests parse `.invalid` names without connecting.

These are not graphical hit-testing or pixel-layout tests. Face-state/width
branches have controlled inputs. Glyph appearance, clipping, font scaling,
physical mouse delivery and exact alignment still require display tests.

Fixtures isolate defaults, mode registries, global maps, window configuration,
buffers and temporary files with dynamic bindings and `unwind-protect`. They
also test cleanup after errors. The canonical environment is `-Q`, not an
arbitrarily customized session. No failures are marked expected or skipped.

References: [ERT environment conventions](https://www.gnu.org/software/emacs/manual/html_node/ert/Tests-and-Their-Environment.html),
[ERT batch execution](https://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-in-Batch-Mode.html),
package docstrings, and Emacs 31.1 Flymake source/public APIs.
