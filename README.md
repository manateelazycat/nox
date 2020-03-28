# Nox

Nox is LSP client that fork from [eglot](https://github.com/joaotavora/eglot), to encode fluency, removed a lot of features that affect performance, such as real-time syntax checking.

And add many other enhancements, such as use posframe instead eldoc show help documentation.

# Install

1. Clone this repository and put nox.el in your load-path

2. Add below configure in your ~/.emacs

```elisp
(require 'nox)

(dolist (hook (list
               'js-mode-hook
               'rust-mode-hook
               'python-mode-hook
               'ruby-mode-hook
               'java-mode-hook
               'sh-mode-hook
               'php-mode-hook
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'haskell-mode-hook
               ))
  (add-hook hook '(lambda () (nox-ensure))))
```
3. Open file, that's all.

<a name="connecting"></a>
# Connecting to a server

`M-x nox` can guess and work out-of-the-box with these servers:

* Javascript: javascript-typescript-stdio
* Rust: rls
* Python: pyls
* Ruby: solargraph
* Java: eclipse-jdt
* Bash: bash-language-server
* PHP: php-language-server
* C/C++: ccls
* Haskell: haskell-ide-engine
* Elm: elm-language-server
* Kotlin: kotlin-language-server
* Go: gopls
* Ocaml: ocaml-language-server
* R: languageserver
* Dart: dart_language_server
* Elixir: elixir-ls
* Ada: ada_language_server
* Scala: metals
* TeX/LaTeX: digestif

I'll add to this list as I test more servers. In the meantime you can
customize `nox-server-programs`:

```lisp
(add-to-list 'nox-server-programs '(foo-mode . ("foo-language-server" "--args")))
```

Let me know how well it works and we can add it to the list.

To skip the guess and always be prompted use `C-u M-x nox`.

## Connecting via TCP

The examples above use a "pipe" to talk to the server, which works
fine on Linux and OSX but in some cases
[*may not work on Windows*][windows-subprocess-hang].

To circumvent this limitation, or if the server doesn't like pipes,
you can use `C-u M-x nox` and give it `server:port` pattern to
connect to a previously started TCP server serving LSP information.

If you don't want to start it manually every time, you can configure
Nox to start it and immediately connect to it.  Ruby's
[solargraph][solargraph] server already works this way out-of-the-box.

For another example, suppose you also wanted start Python's `pyls`
this way:

```lisp
(add-to-list 'nox-server-programs
             `(python-mode . ("pyls" "-v" "--tcp" "--host"
                              "localhost" "--port" :autoport)))
```

You can see that the element associated with `python-mode` is now a
more complicated invocation of the `pyls` program, which requests that
it be started as a server.  Notice the `:autoport` symbol in there: it
is replaced dynamically by a local port believed to be vacant, so that
the ensuing TCP connection finds a listening server.

## Per-project server configuration

Most servers can guess good defaults and will operate nicely
out-of-the-box, but some need to be configured specially via LSP
interfaces.  Additionally, in some situations, you may also want a
particular server to operate differently across different projects.

Per-project settings are realized with Emacs's _directory variables_
and the Elisp variable `nox-workspace-configuration`.  To make a
particular Python project always enable Pyls's snippet support, put a
file named `.dir-locals.el` in the project's root:

```lisp
((python-mode
  . ((nox-workspace-configuration
      . ((:pyls . (:plugins (:jedi_completion (:include_params t)))))))))
```

This tells Emacs that any `python-mode` buffers in that directory
should have a particular buffer-local value of
`nox-workspace-configuration`.  That variable's value should be
_association list_ of _parameter sections_ which are presumably
understood by the server.  In this example, we associate section
`pyls` with the parameters object `(:plugins (:jedi_completion
(:include_params t)))`.

Now, supposing that you also had some Go code in the very same
project, you can configure the Gopls server in the same file.  Adding
a section for `go-mode`, the file's contents become:

```lisp
((python-mode
  . ((nox-workspace-configuration
      . ((:pyls . (:plugins (:jedi_completion (:include_params t))))))))
 (go-mode
  . ((nox-workspace-configuration
      . ((:gopls . (:usePlaceholders t)))))))
```

If you can't afford an actual `.dir-locals.el` file, or if managing
these files becomes cumbersome, the Emacs manual teaches you
programmatic ways to leverage per-directory local variables.

## Handling quirky servers

Some servers need even more special hand-holding to operate correctly.
If your server has some quirk or non-conformity, it's possible to
extend Nox via Elisp to adapt to it.  Here's an example on how to
get [cquery][cquery] working:

```lisp
(add-to-list 'nox-server-programs '((c++ mode c-mode) . (nox-cquery "cquery")))

(defclass nox-cquery (nox-lsp-server) ()
  :documentation "A custom class for cquery's C/C++ langserver.")

(cl-defmethod nox-initialization-options ((server nox-cquery))
  "Passes through required cquery initialization options"
  (let* ((root (car (project-roots (nox--project server))))
         (cache (expand-file-name ".cquery_cached_index/" root)))
    (list :cacheDirectory (file-name-as-directory cache)
          :progressReportFrequencyMs -1)))
```

See `nox.el`'s section on Java's JDT server for an even more
sophisticated example.

<a name="reporting bugs"></a>
# Reporting bugs

Having trouble connecting to a server?  Expected to have a certain
capability supported by it (e.g. completion) but nothing happens?  Or
do you get spurious and annoying errors in an otherwise smooth
operation?  We may have help, so open a [new
issue](https://github.com/manateelazycat/nox/issues) and try to be as
precise and objective about the problem as you can:

1. Try to replicate the problem with **as clean an Emacs run as
   possible**.  This means an empty `.emacs` init file or close to it
   (just loading `nox.el`, `company.el` and `yasnippet.el` for
   example, and you don't even need `use-package.el` to do that).

2. Include the log of **LSP events** and the **stderr output** of the
   server (if any).  You can find the former with `M-x
   nox-events-buffer` and the latter with `M-x nox-stderr-buffer`.
   You run these commands in the buffer where you enabled Nox, but
   if you didn't manage to enable Nox at all (because of some
   bootstrapping problem), you can still find these buffers in your
   buffer list: they're named like `*NOX <project>/<major-mode>
   events*` and `*NOX <project>/<major-mode> stderr*`.

3. If Emacs errored (you saw -- and possibly heard -- an error
   message), make sure you repeat the process using `M-x
   toggle-debug-on-error` so you **get a backtrace** of the error that
   you should also attach to the bug report.

Some more notes: it's understandable that you report it to Nox
first, because that's the user-facing side of the LSP experience in
Emacs, but the outcome may well be that you will have to report the
problem to the server's developers, as is often the case.  But the
problem can very well be on Nox's side, of course, and in that case
we want to fix it!  Also bear in mind that Nox's developers have
limited resources and no way to test all the possible server
combinations, so you'll have to do most of the testing.

<a name="commands"></a>
# Commands and keybindings

Here's a summary of available commands:

- `M-x nox`, as described above;

- `M-x nox-reconnect` reconnects to the server;

- `M-x nox-shutdown` says bye-bye to the server;

- `M-x nox-rename` ask the server to rename the symbol at point;

- `M-x nox-format` asks the server to format buffer or the active
  region;

- `M-x nox-code-actions` asks the server for any code actions at
  point. These may tipically be simple fixes, like deleting an unused
  variable, or fixing an import. Left click on diagnostics to check if
  there are any there;

- `M-x nox-show-doc` show documentation for symbol at point.

- `M-x nox-events-buffer` jumps to the events buffer for debugging
  communication with the server.

- `M-x nox-stderr-buffer` if the LSP server is printing useful debug
information in stderr, jumps to a buffer with these contents.

- `M-x nox-signal-didChangeConfiguration` updates the LSP server
configuration according to the value of the variable
`nox-workspace-configuration`, which you may be set in a
`.dir-locals` file, for example.

There are *no keybindings* specific to Nox, but you can bind stuff
in `nox-mode-map`, which is active as long as Nox is managing a
file in your project. The commands don't need to be Nox-specific,
either:

```
(define-key nox-mode-map (kbd "C-c h") 'nox-help-at-point)
(define-key nox-mode-map (kbd "<f6>") 'xref-find-definitions)
```

<a name="customization"></a>
# Customization

Here's a quick summary of the customization options.  In Nox's
customization group (`M-x customize-group`) there is more
documentation on what these do.

- `nox-autoreconnect`: Control ability to reconnect automatically to
  the LSP server;

- `nox-connect-timeout`: Number of seconds before timing out LSP
  connection attempts;

- `nox-sync-connect`: Control blocking of LSP connection attempts;

- `nox-events-buffer-size`: Control the size of the Nox events
  buffer;

- `nox-ignored-server-capabilites`: LSP server capabilities that
  Nox could use, but won't;

- `nox-put-doc-in-help-buffer`: If non-nil, put eldoc docstrings in
  separate `*nox-help*` buffer;

- `nox-auto-display-help-buffer`: If non-nil, automatically display
  `*nox-help*` buffer;

- `nox-confirm-server-initiated-edits`: If non-nil, ask for confirmation
  before allowing server to edit the source buffer's text;

There are a couple more variables that you can customize via Emacs
lisp:

- `nox-server-programs`: as described [above](#connecting);

- `nox-strict-mode`: Set to `nil` by default, meaning Nox is
  generally lenient about non-conforming servers.  Set this to
  `(disallow-non-standard-keys enforce-required-keys)` when debugging
  servers.

- `nox-server-initialized-hook`: Hook run after server is
  successfully initialized;

- `nox-managed-mode-hook`: Hook run after Nox started or stopped
  managing a buffer.  Use `nox-managed-p` to tell if current buffer
  is still being managed.
