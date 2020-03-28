;;; nox.el --- Nox is LSP Client for Emacs   -*- lexical-binding: t; -*-

;; Filename: nox.el
;; Description: Nox is LSP Client for Emacs
;; Author: João Távora <joaotavora@gmail.com>
;;         Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, João Távora, all rights reserved.
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-28 16:27:25
;; Version: 0.1
;; Last-Updated: 2020-03-28 16:27:25
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/nox.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Nox is LSP Client for Emacs
;;

;;; Installation:
;;
;; Put nox.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'nox)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET nox RET
;;

;;; Change log:
;;
;; 2020/03/28
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile)                      ; for some faces
(require 'warnings)
(require 'xref)
(eval-when-compile
  (require 'subr-x))
(require 'jsonrpc)
(require 'filenotify)
(require 'ert)
(require 'array)
(defvar company-backends) ; forward-declare, but don't require company
(defvar company-tooltip-align-annotations)

;;; Code:

;;; User tweakable stuff
(defgroup nox nil
  "Interaction with Language Server Protocol servers"
  :prefix "nox-"
  :group 'applications)

(defvar nox-server-programs
  '((rust-mode . (nox-rls "rls"))
    (python-mode . ("pyls"))
    ((js-mode typescript-mode) . ("javascript-typescript-stdio"))
    (sh-mode . ("bash-language-server" "start"))
    ((php-mode phps-mode) . ("php" "vendor/felixfbecker/anguage-server/bin/php-language-server.php"))
    ((c++-mode c-mode) . ("ccls"))
    ((caml-mode tuareg-mode reason-mode) . ("ocaml-language-server" "--stdio"))
    (ruby-mode . ("solargraph" "socket" "--port" :autoport))
    (haskell-mode . ("hie-wrapper"))
    (elm-mode . ("elm-language-server"))
    (kotlin-mode . ("kotlin-language-server"))
    (go-mode . ("gopls"))
    ((R-mode ess-r-mode) . ("R" "--slave" "-e" "languageserver::run()"))
    (java-mode . nox--eclipse-jdt-contact)
    (dart-mode . ("dart_language_server"))
    (elixir-mode . ("language_server.sh"))
    (ada-mode . ("ada_language_server"))
    (scala-mode . ("metals-emacs"))
    ((tex-mode context-mode texinfo-mode bibtex-mode) . ("digestif"))
    (dockerfile-mode . ("docker-langserver" "--stdio"))
    (css-mode "css-languageserver" "--stdio")
    (html-mode "html-languageserver" "--stdio")
    (json-mode "json-languageserver" "--stdio"))
  "How the command `nox' guesses the server to start.
An association list of (MAJOR-MODE . CONTACT) pairs.  MAJOR-MODE
is a mode symbol, or a list of mode symbols.  The associated
CONTACT specifies how to connect to a server for managing buffers
of those modes.  CONTACT can be:

* In the most common case, a list of strings (PROGRAM [ARGS...]).
  PROGRAM is called with ARGS and is expected to serve LSP requests
  over the standard input/output channels.

* A list (HOST PORT [TCP-ARGS...]) where HOST is a string and PORT is
  na positive integer number for connecting to a server via TCP.
  Remaining ARGS are passed to `open-network-stream' for
  upgrading the connection with encryption or other capabilities.

* A list (PROGRAM [ARGS...] :autoport [MOREARGS...]), whereby a
  combination of the two previous options is used..  First, an
  attempt is made to find an available server port, then PROGRAM
  is launched with ARGS; the `:autoport' keyword substituted for
  that number; and MOREARGS.  Nox then attempts to to establish
  a TCP connection to that port number on the localhost.

* A cons (CLASS-NAME . INITARGS) where CLASS-NAME is a symbol
  designating a subclass of `nox-lsp-server', for representing
  experimental LSP servers.  INITARGS is a keyword-value plist
  used to initialize CLASS-NAME, or a plain list interpreted as
  the previous descriptions of CONTACT, in which case it is
  converted to produce a plist with a suitable :PROCESS initarg
  to CLASS-NAME.  The class `nox-lsp-server' descends
  `jsonrpc-process-connection', which you should see for the
  semantics of the mandatory :PROCESS argument.

* A function of a single argument producing any of the above
  values for CONTACT.  The argument's value is non-nil if the
  connection was requested interactively (e.g. from the `nox'
  command), and nil if it wasn't (e.g. from `nox-ensure').  If
  the call is interactive, the function can ask the user for
  hints on finding the required programs, etc.  Otherwise, it
  should not ask the user for any input, and return nil or signal
  an error if it can't produce a valid CONTACT.")

(defcustom nox-autoreconnect 3
  "Control ability to reconnect automatically to the LSP server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))

(defcustom nox-connect-timeout 30
  "Number of seconds before timing out LSP connection attempts.
If nil, never time out."
  :type 'number)

(defcustom nox-sync-connect 3
  "Control blocking of LSP connection attempts.
If t, block for `nox-connect-timeout' seconds.  A positive
integer number means block for that many seconds, and then wait
for the connection in the background.  nil has the same meaning
as 0, i.e. don't block at all."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))

(defcustom nox-autoshutdown nil
  "If non-nil, shut down server after killing last managed buffer."
  :type 'boolean)

(defcustom nox-send-changes-idle-time 0.5
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :type 'number)

(defcustom nox-events-buffer-size 2000000
  "Control the size of the Nox events buffer.
If a number, don't let the buffer grow larger than that many
characters.  If 0, don't use an event's buffer at all.  If nil,
let the buffer grow forever."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Number of characters")))

(defcustom nox-confirm-server-initiated-edits 'confirm
  "Non-nil if server-initiated edits should be confirmed with user."
  :type '(choice (const :tag "Don't show confirmation prompt" nil)
                 (symbol :tag "Show confirmation prompt" 'confirm)))

(defcustom nox-ignored-server-capabilites (list)
  "LSP server capabilities that Nox could use, but won't.
You could add, for instance, the symbol
`:documentHighlightProvider' to prevent automatic highlighting
under cursor."
  :type '(repeat
          (choice
           (const :tag "Documentation on hover" :hoverProvider)
           (const :tag "Code completion" :completionProvider)
           (const :tag "Function signature help" :signatureHelpProvider)
           (const :tag "Go to definition" :definitionProvider)
           (const :tag "Go to type definition" :typeDefinitionProvider)
           (const :tag "Go to implementation" :implementationProvider)
           (const :tag "Go to declaration" :implementationProvider)
           (const :tag "Find references" :referencesProvider)
           (const :tag "Highlight symbols automatically" :documentHighlightProvider)
           (const :tag "List symbols in buffer" :documentSymbolProvider)
           (const :tag "List symbols in workspace" :workspaceSymbolProvider)
           (const :tag "Execute code actions" :codeActionProvider)
           (const :tag "Code lens" :codeLensProvider)
           (const :tag "Format buffer" :documentFormattingProvider)
           (const :tag "Format portion of buffer" :documentRangeFormattingProvider)
           (const :tag "On-type formatting" :documentOnTypeFormattingProvider)
           (const :tag "Rename symbol" :renameProvider)
           (const :tag "Highlight links in document" :documentLinkProvider)
           (const :tag "Decorate color references" :colorProvider)
           (const :tag "Fold regions of buffer" :foldingRangeProvider)
           (const :tag "Execute custom commands" :executeCommandProvider)
           (symbol :tag "Other"))))

(defcustom nox-doc-tooltip-font-size "14"
  "The font size of documentation tooltip."
  :type 'string)

(defcustom nox-doc-tooltip-border-width 15
  "The border width of nox tooltip, default is 15 px."
  :type 'integer)

(defcustom nox-doc-tooltip-timeout 30
  "The timeout of nox tooltip show time, in seconds."
  :type 'integer)

(defcustom nox-doc-tooltip-name "*nox doc*"
  "The name of nox tooltip name."
  :type 'string)

(defcustom nox-candidate-annotation-limit 80
  "The limit of annotation."
  :type 'integer)

;;; Constants
;;;
(defconst nox--symbol-kind-names
  `((1 . "File") (2 . "Module")
    (3 . "Namespace") (4 . "Package") (5 . "Class")
    (6 . "Method") (7 . "Property") (8 . "Field")
    (9 . "Constructor") (10 . "Enum") (11 . "Interface")
    (12 . "Function") (13 . "Variable") (14 . "Constant")
    (15 . "String") (16 . "Number") (17 . "Boolean")
    (18 . "Array") (19 . "Object") (20 . "Key")
    (21 . "Null") (22 . "EnumMember") (23 . "Struct")
    (24 . "Event") (25 . "Operator") (26 . "TypeParameter")))

(defconst nox--kind-names
  `((1 . "Text") (2 . "Method") (3 . "Fn") (4 . "Ctor")
    (5 . "Field") (6 . "Var") (7 . "Class") (8 . "I/F")
    (9 . "Mod") (10 . "Prop") (11 . "Unit") (12 . "Value")
    (13 . "Enum") (14 . "K/W") (15 . "Sn") (16 . "Color")
    (17 . "File") (18 . "Refer")))

(defconst nox--{} (make-hash-table) "The empty JSON object.")

;;; Message verification helpers
;;;
(eval-and-compile
  (defvar nox--lsp-interface-alist
    `(
      (CodeAction (:title) (:kind :diagnostics :edit :command))
      (ConfigurationItem () (:scopeUri :section))
      (Command (:title :command) (:arguments))
      (CompletionItem (:label)
                      (:kind :detail :documentation :deprecated :preselect
                             :sortText :filterText :insertText :insertTextFormat
                             :textEdit :additionalTextEdits :commitCharacters
                             :command :data))
      (Diagnostic (:range :message) (:severity :code :source :relatedInformation))
      (DocumentHighlight (:range) (:kind))
      (FileSystemWatcher (:globPattern) (:kind))
      (Hover (:contents) (:range))
      (InitializeResult (:capabilities) (:serverInfo))
      (Location (:uri :range))
      (LogMessageParams (:type :message))
      (MarkupContent (:kind :value))
      (ParameterInformation (:label) (:documentation))
      (Position (:line :character))
      (Range (:start :end))
      (Registration (:id :method) (:registerOptions))
      (Registration (:id :method) (:registerOptions))
      (ResponseError (:code :message) (:data))
      (ShowMessageParams (:type :message))
      (ShowMessageRequestParams (:type :message) (:actions))
      (SignatureHelp (:signatures) (:activeSignature :activeParameter))
      (SignatureInformation (:label) (:documentation :parameters))
      (SymbolInformation (:name :kind :location) (:deprecated :containerName))
      (TextDocumentEdit (:textDocument :edits) ())
      (TextEdit (:range :newText))
      (VersionedTextDocumentIdentifier (:uri :version) ())
      (WorkspaceEdit () (:changes :documentChanges))
      )
    "Alist (INTERFACE-NAME . INTERFACE) of known external LSP interfaces.

INTERFACE-NAME is a symbol designated by the spec as
\"interface\".  INTERFACE is a list (REQUIRED OPTIONAL) where
REQUIRED and OPTIONAL are lists of keyword symbols designating
field names that must be, or may be, respectively, present in a
message adhering to that interface.

Here's what an element of this alist might look like:

    (CreateFile . ((:kind :uri) (:options)))"))

(eval-and-compile
  (defvar nox-strict-mode (if load-file-name '()
                            '(disallow-non-standard-keys
                              ))
    "How strictly to check LSP interfaces at compile- and run-time.

Value is a list of symbols (if the list is empty, no checks are
performed).

If the symbol `disallow-non-standard-keys' is present, an error
is raised if any extraneous fields are sent by the server.  At
compile-time, a warning is raised if a destructuring spec
includes such a field.

If the symbol `enforce-required-keys' is present, an error is
raised if any required fields are missing from the message sent
from the server.  At compile-time, a warning is raised if a
destructuring spec doesn't use such a field.

If the symbol `enforce-optional-keys' is present, nothing special
happens at run-time.  At compile-time, a warning is raised if a
destructuring spec doesn't use all optional fields.

If the symbol `disallow-unknown-methods' is present, Nox warns
on unknown notifications and errors on unknown requests.
"))

(defun nox--plist-keys (plist)
  (cl-loop for (k _v) on plist by #'cddr collect k))

(defun nox--call-with-interface (interface object fn)
  "Call FN, checking that OBJECT conforms to INTERFACE."
  (when-let ((missing (and (memq 'enforce-required-keys nox-strict-mode)
                           (cl-set-difference (car (cdr interface))
                                              (nox--plist-keys object)))))
    (nox--error "A `%s' must have %s" (car interface) missing))
  (when-let ((excess (and (memq 'disallow-non-standard-keys nox-strict-mode)
                          (cl-set-difference
                           (nox--plist-keys object)
                           (append (car (cdr interface)) (cadr (cdr interface)))))))
    (nox--error "A `%s' mustn't have %s" (car interface) excess))
  (funcall fn))

(eval-and-compile
  (defun nox--keywordize-vars (vars)
    (mapcar (lambda (var) (intern (format ":%s" var))) vars))

  (defun nox--check-interface (interface-name vars)
    (let ((interface
           (assoc interface-name nox--lsp-interface-alist)))
      (cond (interface
             (let ((too-many
                    (and
                     (memq 'disallow-non-standard-keys nox-strict-mode)
                     (cl-set-difference
                      (nox--keywordize-vars vars)
                      (append (car (cdr interface))
                              (cadr (cdr interface))))))
                   (ignored-required
                    (and
                     (memq 'enforce-required-keys nox-strict-mode)
                     (cl-set-difference
                      (car (cdr interface))
                      (nox--keywordize-vars vars))))
                   (missing-out
                    (and
                     (memq 'enforce-optional-keys nox-strict-mode)
                     (cl-set-difference
                      (cadr (cdr interface))
                      (nox--keywordize-vars vars)))))
               (when too-many (byte-compile-warn
                               "Destructuring for %s has extraneous %s"
                               interface-name too-many))
               (when ignored-required (byte-compile-warn
                                       "Destructuring for %s ignores required %s"
                                       interface-name ignored-required))
               (when missing-out (byte-compile-warn
                                  "Destructuring for %s is missing out on %s"
                                  interface-name missing-out))))
            (t
             (byte-compile-warn "Unknown LSP interface %s" interface-name))))))

(cl-defmacro nox--dbind (vars object &body body)
  "Destructure OBJECT of binding VARS in BODY.
VARS is ([(INTERFACE)] SYMS...)
Honour `nox-strict-mode'."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((interface-name (if (consp (car vars))
                            (car (pop vars))))
        (object-once (make-symbol "object-once"))
        (fn-once (make-symbol "fn-once")))
    (cond (interface-name
           (nox--check-interface interface-name vars)
           `(let ((,object-once ,object))
              (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                (nox--call-with-interface (assoc ',interface-name
                                                 nox--lsp-interface-alist)
                                          ,object-once (lambda ()
                                                         ,@body)))))
          (t
           `(let ((,object-once ,object)
                  (,fn-once (lambda (,@vars) ,@body)))
              (if (memq 'disallow-non-standard-keys nox-strict-mode)
                  (cl-destructuring-bind (&key ,@vars) ,object-once
                    (funcall ,fn-once ,@vars))
                (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                  (funcall ,fn-once ,@vars))))))))


(cl-defmacro nox--lambda (cl-lambda-list &body body)
  "Function of args CL-LAMBDA-LIST for processing INTERFACE objects.
Honour `nox-strict-mode'."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (nox--dbind ,cl-lambda-list ,e ,@body))))

(cl-defmacro nox--dcase (obj &rest clauses)
  "Like `pcase', but for the LSP object OBJ.
CLAUSES is a list (DESTRUCTURE FORMS...) where DESTRUCTURE is
treated as in `nox-dbind'."
  (declare (indent 1) (debug (sexp &rest (sexp &rest form))))
  (let ((obj-once (make-symbol "obj-once")))
    `(let ((,obj-once ,obj))
       (cond
        ,@(cl-loop
           for (vars . body) in clauses
           for vars-as-keywords = (nox--keywordize-vars vars)
           for interface-name = (if (consp (car vars))
                                    (car (pop vars)))
           for condition =
           (cond (interface-name
                  (nox--check-interface interface-name vars)
                  ;; In this mode, in runtime, we assume
                  ;; `nox-strict-mode' is fully on, otherwise we
                  ;; can't disambiguate between certain types.
                  `(let* ((interface
                           (or (assoc ',interface-name nox--lsp-interface-alist)
                               (nox--error "Unknown LSP interface %s"
                                           ',interface-name)))
                          (object-keys (nox--plist-keys ,obj-once))
                          (required-keys (car (cdr interface))))
                     (and (null (cl-set-difference required-keys object-keys))
                          (null (cl-set-difference
                                 (cl-set-difference object-keys required-keys)
                                 (cadr (cdr interface)))))))
                 (t
                  ;; In this interface-less mode we don't check
                  ;; `nox-strict-mode' at all: just check that the object
                  ;; has all the keys the user wants to destructure.
                  `(null (cl-set-difference
                          ',vars-as-keywords
                          (nox--plist-keys ,obj-once)))))
           collect `(,condition
                     (cl-destructuring-bind (&key ,@vars &allow-other-keys)
                         ,obj-once
                       ,@body)))
        (t
         (nox--error "%s didn't match any of %s"
                     ,obj-once
                     ',(mapcar #'car clauses)))))))

;;; API (WORK-IN-PROGRESS!)
;;;
(cl-defmacro nox--with-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro nox--widening (&rest body)
  "Save excursion and restriction. Widen. Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))

(cl-defgeneric nox-handle-request (server method &rest params)
  "Handle SERVER's METHOD request with PARAMS.")

(cl-defgeneric nox-handle-notification (server method &rest params)
  "Handle SERVER's METHOD notification with PARAMS.")

(cl-defgeneric nox-execute-command (server command arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(cl-defgeneric nox-initialization-options (server)
  "JSON object to send under `initializationOptions'"
  (:method (_s) nil))                   ; blank default

(cl-defgeneric nox-register-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (nox--warn "Server tried to register unsupported capability `%s'"
              method)))

(cl-defgeneric nox-unregister-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (nox--warn "Server tried to unregister unsupported capability `%s'"
              method)))

(cl-defgeneric nox-client-capabilities (server)
  "What the NOX LSP client supports for SERVER."
  (:method (_s)
           (list
            :workspace (list
                        :applyEdit t
                        :executeCommand `(:dynamicRegistration :json-false)
                        :workspaceEdit `(:documentChanges :json-false)
                        :didChangeWatchedFiles `(:dynamicRegistration t)
                        :symbol `(:dynamicRegistration :json-false)
                        :configuration t)
            :textDocument
            (list
             :synchronization (list
                               :dynamicRegistration :json-false
                               :willSave t :willSaveWaitUntil t :didSave t)
             :completion      (list :dynamicRegistration :json-false
                                    :completionItem
                                    `(:snippetSupport t)
                                    :contextSupport t)
             :hover              (list :dynamicRegistration :json-false
                                       :contentFormat ["markdown" "plaintext"])
             :signatureHelp      (list :dynamicRegistration :json-false
                                       :signatureInformation
                                       `(:parameterInformation
                                         (:labelOffsetSupport t)))
             :references         `(:dynamicRegistration :json-false)
             :definition         `(:dynamicRegistration :json-false)
             :declaration        `(:dynamicRegistration :json-false)
             :implementation     `(:dynamicRegistration :json-false)
             :typeDefinition     `(:dynamicRegistration :json-false)
             :documentSymbol     (list
                                  :dynamicRegistration :json-false
                                  :symbolKind `(:valueSet
                                                [,@(mapcar
                                                    #'car nox--symbol-kind-names)]))
             :documentHighlight  `(:dynamicRegistration :json-false)
             :codeAction         (list
                                  :dynamicRegistration :json-false
                                  :codeActionLiteralSupport
                                  '(:codeActionKind
                                    (:valueSet [])))
             :formatting         `(:dynamicRegistration :json-false)
             :rangeFormatting    `(:dynamicRegistration :json-false)
             :rename             `(:dynamicRegistration :json-false)
             :publishDiagnostics `(:relatedInformation :json-false))
            :experimental (list))))

(defclass nox-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor nox--project-nickname
    :reader nox-project-nickname)
   (major-mode
    :documentation "Major mode symbol."
    :accessor nox--major-mode)
   (capabilities
    :documentation "JSON object containing server capabilities."
    :accessor nox--capabilities)
   (server-info
    :documentation "JSON object containing server info."
    :accessor nox--server-info)
   (shutdown-requested
    :documentation "Flag set when server is shutting down."
    :accessor nox--shutdown-requested)
   (project
    :documentation "Project associated with server."
    :accessor nox--project)
   (spinner
    :documentation "List (ID DOING-WHAT DONE-P) representing server progress."
    :initform `(nil nil t) :accessor nox--spinner)
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor nox--inhibit-autoreconnect)
   (file-watches
    :documentation "Map ID to list of WATCHES for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor nox--file-watches)
   (managed-buffers
    :documentation "List of buffers managed by server."
    :accessor nox--managed-buffers)
   (saved-initargs
    :documentation "Saved initargs for reconnection purposes."
    :accessor nox--saved-initargs)
   (inferior-process
    :documentation "Server subprocess started automatically."
    :accessor nox--inferior-process))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")


;;; Process management
(defvar nox--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defun nox-shutdown (server &optional _interactive timeout preserve-buffers)
  "Politely ask SERVER to quit.
Interactively, read SERVER from the minibuffer unless there is
only one and it's managing the current buffer.

Forcefully quit it if it doesn't respond within TIMEOUT seconds.
Don't leave this function with the server still running.

If PRESERVE-BUFFERS is non-nil (interactively, when called with a
prefix argument), do not kill events and output buffers of
SERVER.  ."
  (interactive (list (nox--read-server "Shutdown which server"
                                       (nox-current-server))
                     t nil current-prefix-arg))
  (nox--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (progn
        (setf (nox--shutdown-requested server) t)
        (jsonrpc-request server :shutdown nox--{}
                         :timeout (or timeout 1.5))
        (jsonrpc-notify server :exit nox--{}))
    ;; Now ask jsonrpc.el to shut down the server.
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers (kill-buffer (jsonrpc-events-buffer server)))))

(defun nox--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `nox--managed-mode' where appropriate.
  (dolist (buffer (nox--managed-buffers server))
    (let (;; Avoid duplicate shutdowns (github#389)
          (nox-autoshutdown nil))
      (nox--with-live-buffer buffer (nox--managed-mode-off))))
  ;; Kill any expensive watches
  (maphash (lambda (_id watches)
             (mapcar #'file-notify-rm-watch watches))
           (nox--file-watches server))
  ;; Kill any autostarted inferior processes
  (when-let (proc (nox--inferior-process server))
    (delete-process proc))
  ;; Sever the project/server relationship for `server'
  (setf (gethash (nox--project server) nox--servers-by-project)
        (delq server
              (gethash (nox--project server) nox--servers-by-project)))
  (cond ((nox--shutdown-requested server)
         t)
        ((not (nox--inhibit-autoreconnect server))
         (nox--warn "Reconnecting after unexpected server exit.")
         (nox-reconnect server))
        ((timerp (nox--inhibit-autoreconnect server))
         (nox--warn "Not auto-reconnecting, last one didn't last long."))))

(defun nox--all-major-modes ()
  "Return all known major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defvar nox--command-history nil
  "History of CONTACT arguments to `nox'.")

(defun nox--guess-contact (&optional interactive)
  "Helper for `nox'.
Return (MANAGED-MODE PROJECT CLASS CONTACT).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((guessed-mode (if buffer-file-name major-mode))
         (managed-mode
          (cond
           ((and interactive
                 (or (>= (prefix-numeric-value current-prefix-arg) 16)
                     (not guessed-mode)))
            (intern
             (completing-read
              "[nox] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (nox--all-major-modes)) nil t
              (symbol-name guessed-mode) nil (symbol-name guessed-mode) nil)))
           ((not guessed-mode)
            (nox--error "Can't guess mode to manage for `%s'" (current-buffer)))
           (t guessed-mode)))
         (project (or (project-current) `(transient . ,default-directory)))
         (guess (cdr (assoc managed-mode nox-server-programs
                            (lambda (m1 m2)
                              (cl-find
                               m2 (if (listp m1) m1 (list m1))
                               :test #'provided-mode-derived-p)))))
         (guess (if (functionp guess)
                    (funcall guess interactive)
                  guess))
         (class (or (and (consp guess) (symbolp (car guess))
                         (prog1 (car guess) (setq guess (cdr guess))))
                    'nox-lsp-server))
         (program (and (listp guess) (stringp (car guess)) (car guess)))
         (base-prompt
          (and interactive
               "Enter program to execute (or <host>:<port>): "))
         (program-guess
          (and program
               (combine-and-quote-strings (cl-subst ":autoport:"
                                                    :autoport guess))))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[nox] Sorry, couldn't guess for `%s'!\n%s"
                              managed-mode base-prompt))
                     ((and program (not (executable-find program)))
                      (concat (format "[nox] I guess you want to run `%s'"
                                      program-guess)
                              (format ", but I can't find `%s' in PATH!" program)
                              "\n" base-prompt)))))
         (contact
          (or (and prompt
                   (let ((s (read-shell-command
                             prompt
                             program-guess
                             'nox-command-history)))
                     (if (string-match "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                       (string-trim s))
                         (list (match-string 1 s)
                               (string-to-number (match-string 2 s)))
                       (cl-subst
                        :autoport ":autoport:" (split-string-and-unquote s)
                        :test #'equal))))
              guess
              (nox--error "Couldn't guess for `%s'!" managed-mode))))
    (list managed-mode project class contact)))

;;;###autoload
(defun nox (managed-major-mode project class contact &optional interactive)
  "Manage a project with a Language Server Protocol (LSP) server.

The LSP server of CLASS started (or contacted) via CONTACT.  If
this operation is successful, current *and future* file buffers
of MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions',
`completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CLASS and CONTACT from
`nox-server-programs' and PROJECT from `project-current'.  If
it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

CLASS is a subclass of symbol `nox-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `nox-server-programs', which see.

INTERACTIVE is t if called interactively."
  (interactive (append (nox--guess-contact t) '(t)))
  (let* ((current-server (nox-current-server))
         (live-p (and current-server (jsonrpc-running-p current-server))))
    (if (and live-p
             interactive
             (y-or-n-p "[nox] Live process found, reconnect instead? "))
        (nox-reconnect current-server interactive)
      (when live-p (ignore-errors (nox-shutdown current-server)))
      (nox--connect managed-major-mode project class contact))))

(defun nox-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (nox--current-server-or-lose) t))
  (when (jsonrpc-running-p server)
    (ignore-errors (nox-shutdown server interactive nil 'preserve-buffers)))
  (nox--connect (nox--major-mode server)
                (nox--project server)
                (eieio-object-class-name server)
                (nox--saved-initargs server))
  (nox--message "Reconnected!"))

(defvar nox--managed-mode)              ; forward decl

;;;###autoload
(defun nox-ensure ()
  "Start Nox session for current buffer if there isn't one."
  (let ((buffer (current-buffer)))
    (cl-labels
        ((maybe-connect
          ()
          (remove-hook 'post-command-hook #'maybe-connect nil)
          (nox--with-live-buffer buffer
            (unless nox--managed-mode
              (apply #'nox--connect (nox--guess-contact))))))
      (when buffer-file-name
        (add-hook 'post-command-hook #'maybe-connect 'append nil)
        ))))

(defun nox-events-buffer (server)
  "Display events buffer for SERVER."
  (interactive (list (nox--current-server-or-lose)))
  (display-buffer (jsonrpc-events-buffer server)))

(defun nox-stderr-buffer (server)
  "Display stderr buffer for SERVER."
  (interactive (list (nox--current-server-or-lose)))
  (display-buffer (jsonrpc-stderr-buffer server)))

(defun nox-forget-pending-continuations (server)
  "Forget pending requests for SERVER."
  (interactive (list (nox--current-server-or-lose)))
  (jsonrpc-forget-pending-continuations server))

(defvar nox-connect-hook nil "Hook run after connecting in `nox--connect'.")

(defvar nox-server-initialized-hook
  '(nox-signal-didChangeConfiguration)
  "Hook run after server is successfully initialized.
Each function is passed the server as an argument")

(defun nox--connect (managed-major-mode project class contact)
  "Connect to MANAGED-MAJOR-MODE, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (car (project-roots project)))
         (nickname (file-name-base (directory-file-name default-directory)))
         (readable-name (format "NOX (%s/%s)" nickname managed-major-mode))
         autostart-inferior-process
         (contact (if (functionp contact) (funcall contact) contact))
         (initargs
          (cond ((keywordp (car contact)) contact)
                ((integerp (cadr contact))
                 `(:process ,(lambda ()
                               (apply #'open-network-stream
                                      readable-name nil
                                      (car contact) (cadr contact)
                                      (cddr contact)))))
                ((and (stringp (car contact)) (memq :autoport contact))
                 `(:process ,(lambda ()
                               (pcase-let ((`(,connection . ,inferior)
                                            (nox--inferior-bootstrap
                                             readable-name
                                             contact)))
                                 (setq autostart-inferior-process inferior)
                                 connection))))
                ((stringp (car contact))
                 `(:process
                   ,(lambda ()
                      (let ((default-directory default-directory))
                        (make-process
                         :name readable-name
                         :command contact
                         :connection-type 'pipe
                         :coding 'utf-8-emacs-unix
                         :noquery t
                         :stderr (get-buffer-create
                                  (format "*%s stderr*" readable-name)))))))))
         (spread (lambda (fn) (lambda (server method params)
                            (apply fn server method (append params nil)))))
         (server
          (apply
           #'make-instance class
           :name readable-name
           :events-buffer-scrollback-size nox-events-buffer-size
           :notification-dispatcher (funcall spread #'nox-handle-notification)
           :request-dispatcher (funcall spread #'nox-handle-request)
           :on-shutdown #'nox--on-shutdown
           initargs))
         (cancelled nil)
         (tag (make-symbol "connected-catch-tag")))
    (setf (nox--saved-initargs server) initargs)
    (setf (nox--project server) project)
    (setf (nox--project-nickname server) nickname)
    (setf (nox--major-mode server) managed-major-mode)
    (setf (nox--inferior-process server) autostart-inferior-process)
    ;; Now start the handshake.  To honour `nox-sync-connect'
    ;; maybe-sync-maybe-async semantics we use `jsonrpc-async-request'
    ;; and mimic most of `jsonrpc-request'.
    (unwind-protect
        (condition-case _quit
            (let ((retval
                   (catch tag
                     (jsonrpc-async-request
                      server
                      :initialize
                      (list :processId (unless (eq (jsonrpc-process-type server)
                                                   'network)
                                         (emacs-pid))
                            :rootPath (expand-file-name default-directory)
                            :rootUri (nox--path-to-uri default-directory)
                            :initializationOptions (nox-initialization-options
                                                    server)
                            :capabilities (nox-client-capabilities server))
                      :success-fn
                      (nox--lambda ((InitializeResult) capabilities serverInfo)
                        (unless cancelled
                          (push server
                                (gethash project nox--servers-by-project))
                          (setf (nox--capabilities server) capabilities)
                          (setf (nox--server-info server) serverInfo)
                          (jsonrpc-notify server :initialized nox--{})
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              ;; No need to pass SERVER as an argument: it has
                              ;; been registered in `nox--servers-by-project',
                              ;; so that it can be found (and cached) from
                              ;; `nox--maybe-activate-editing-mode' in any
                              ;; managed buffer.
                              (nox--maybe-activate-editing-mode)))
                          (setf (nox--inhibit-autoreconnect server)
                                (cond
                                 ((booleanp nox-autoreconnect)
                                  (not nox-autoreconnect))
                                 ((cl-plusp nox-autoreconnect)
                                  (run-with-timer
                                   nox-autoreconnect nil
                                   (lambda ()
                                     (setf (nox--inhibit-autoreconnect server)
                                           (null nox-autoreconnect)))))))
                          (let ((default-directory (car (project-roots project)))
                                (major-mode managed-major-mode))
                            (hack-dir-local-variables-non-file-buffer)
                            (run-hook-with-args 'nox-connect-hook server)
                            (run-hook-with-args 'nox-server-initialized-hook server))
                          (nox--message "Connected server with project: %s" (nox-project-nickname server))
                          (when tag (throw tag t))))
                      :timeout nox-connect-timeout
                      :error-fn (nox--lambda ((ResponseError) code message)
                                  (unless cancelled
                                    (jsonrpc-shutdown server)
                                    (let ((msg (format "%s: %s" code message)))
                                      (if tag (throw tag `(error . ,msg))
                                        (nox--error msg)))))
                      :timeout-fn (lambda ()
                                    (unless cancelled
                                      (jsonrpc-shutdown server)
                                      (let ((msg (format "Timed out")))
                                        (if tag (throw tag `(error . ,msg))
                                          (nox--error msg))))))
                     (cond ((numberp nox-sync-connect)
                            (accept-process-output nil nox-sync-connect))
                           (nox-sync-connect
                            (while t (accept-process-output nil 30)))))))
              (pcase retval
                (`(error . ,msg) (nox--error msg))
                (`nil (nox--message "Waiting in background for server %s" (jsonrpc-name server))
                      nil)
                (_ server)))
          (quit (jsonrpc-shutdown server) (setq cancelled 'quit)))
      (setq tag nil))))

(defun nox--inferior-bootstrap (name contact &optional connect-args)
  "Use CONTACT to start a server, then connect to it.
Return a cons of two process objects (CONNECTION . INFERIOR).
Name both based on NAME.
CONNECT-ARGS are passed as additional arguments to
`open-network-stream'."
  (let* ((port-probe (make-network-process :name "nox-port-probe-dummy"
                                           :server t
                                           :host "localhost"
                                           :service 0))
         (port-number (unwind-protect
                          (process-contact port-probe :service)
                        (delete-process port-probe)))
         inferior connection)
    (unwind-protect
        (progn
          (setq inferior
                (make-process
                 :name (format "autostart-inferior-%s" name)
                 :stderr (format "*%s stderr*" name)
                 :noquery t
                 :command (cl-subst
                           (format "%s" port-number) :autoport contact)))
          (setq connection
                (cl-loop
                 repeat 10 for i from 1
                 do (accept-process-output nil 0.5)
                 while (process-live-p inferior)
                 do (nox--message
                     "Trying to connect to localhost and port %s (attempt %s)"
                     port-number i)
                 thereis (ignore-errors
                           (apply #'open-network-stream
                                  (format "autoconnect-%s" name)
                                  nil
                                  "localhost" port-number connect-args))))
          (cons connection inferior))
      (cond ((and (process-live-p connection)
                  (process-live-p inferior))
             (nox--message "Done, connected to %s!" port-number))
            (t
             (when inferior (delete-process inferior))
             (when connection (delete-process connection))
             (nox--error "Could not start and connect to server%s"
                         (if inferior
                             (format " started with %s"
                                     (process-command inferior))
                           "!")))))))

;;; Helpers (move these to API?)
;;;
(defun nox--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[nox] %s" (apply #'format format args)))

(defun nox--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[nox] %s" (apply #'format format args)))

(defun nox--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'nox--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'nox (apply #'format format args) :warning)))

(defun nox-current-column () (- (point) (point-at-bol)))

(defvar nox-current-column-function #'nox-current-column
  "Function to calculate the current column.

This is the inverse operation of
`nox-move-to-column-function' (which see).  It is a function of
no arguments returning a column number.  For buffers managed by
fully LSP-compliant servers, this should be set to
`nox-lsp-abiding-column', and `nox-current-column' (the default)
for all others.")

(defun nox-lsp-abiding-column ()
  "Calculate current COLUMN as defined by the LSP spec."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (point) 'utf-16 t))
        2)
     2))

(defun nox--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (nox--widening
   (list :line (1- (line-number-at-pos pos t)) ; F!@&#$CKING OFF-BY-ONE
         :character (progn (when pos (goto-char pos))
                           (funcall nox-current-column-function)))))

(defvar nox-move-to-column-function #'nox-move-to-column
  "Function to move to a column reported by the LSP server.

According to the standard, LSP column/character offsets are based
on a count of UTF-16 code units, not actual visual columns.  So
when LSP says position 3 of a line containing just \"aXbc\",
where X is a multi-byte character, it actually means `b', not
`c'. However, many servers don't follow the spec this closely.

For buffers managed by fully LSP-compliant servers, this should
be set to `nox-move-to-lsp-abiding-column', and
`nox-move-to-column' (the default) for all others.")

(defun nox-move-to-column (column)
  "Move to COLUMN without closely following the LSP spec."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP columns in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (line-beginning-position) column)
                  (line-end-position))))

(defun nox-move-to-lsp-abiding-column (column)
  "Move to COLUMN abiding by the LSP spec."
  (cl-loop
   initially (move-to-column column)
   with lbp = (line-beginning-position)
   for diff = (- column
                 (/ (- (length (encode-coding-region lbp (point) 'utf-16 t))
                       2)
                    2))
   until (zerop diff)
   do (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))))

(defun nox--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (goto-char (point-min))
    (forward-line (min most-positive-fixnum
                       (plist-get pos-plist :line)))
    (unless (eobp) ;; if line was excessive leave point at eob
      (let ((tab-width 1)
            (col (plist-get pos-plist :character)))
        (unless (wholenump col)
          (nox--warn
           "Caution: LSP server sent invalid character position %s. Using 0 instead."
           col)
          (setq col 0))
        (funcall nox-move-to-column-function col)))
    (if marker (copy-marker (point-marker)) (point))))

(defun nox--path-to-uri (path)
  "URIfy PATH."
  (url-hexify-string
   (concat "file://" (if (eq system-type 'windows-nt) "/") (file-truename path))
   url-path-allowed-chars))

(defun nox--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-filename (url-generic-parse-url (url-unhex-string uri)))))
    (if (eq system-type 'windows-nt) (substring retval 1) retval)))

(defun nox--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list (string-trim markup)
                                          (intern "gfm-view-mode"))
                 (list (plist-get markup :value)
                       (pcase (plist-get markup :kind)
                         ("markdown" 'gfm-view-mode)
                         (_ major-mode))))))
    (with-temp-buffer
      (insert string)
      (ignore-errors (delay-mode-hooks (funcall mode)))
      (font-lock-ensure)
      (buffer-string))))

(defun nox--server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (unless (cl-some (lambda (feat)
                     (memq feat nox-ignored-server-capabilites))
                   feats)
    (cl-loop for caps = (nox--capabilities (nox--current-server-or-lose))
             then (cadr probe)
             for (feat . more) on feats
             for probe = (plist-member caps feat)
             if (not probe) do (cl-return nil)
             if (eq (cadr probe) :json-false) do (cl-return nil)
             if (not (listp (cadr probe))) do (cl-return (if more nil (cadr probe)))
             finally (cl-return (or (cadr probe) t)))))

(defun nox--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((st (plist-get range :start))
         (beg (nox--lsp-position-to-point st markers))
         (end (nox--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

(defun nox--read-server (prompt &optional dont-if-just-the-one)
  "Read a running Nox server from minibuffer using PROMPT.
If DONT-IF-JUST-THE-ONE and there's only one server, don't prompt
and just return it.  PROMPT shouldn't end with a question mark."
  (let ((servers (cl-loop for servers
                          being hash-values of nox--servers-by-project
                          append servers))
        (name (lambda (srv)
                (format "%s/%s" (nox-project-nickname srv)
                        (nox--major-mode srv)))))
    (cond ((null servers)
           (nox--error "No servers!"))
          ((or (cdr servers) (not dont-if-just-the-one))
           (let* ((default (when-let ((current (nox-current-server)))
                             (funcall name current)))
                  (read (completing-read
                         (if default
                             (format "%s (default %s)? " prompt default)
                           (concat prompt "? "))
                         (mapcar name servers)
                         nil t
                         nil nil
                         default)))
             (cl-find read servers :key name :test #'equal)))
          (t (car servers)))))

;;; Minor modes
;;;
(defvar nox-mode-map (make-sparse-keymap))

(defvar-local nox--saved-bindings nil
  "Bindings saved by `nox--setq-saving'.")

(defvar nox-stay-out-of '()
  "List of Emacs things that Nox should try to stay of.
Before Nox starts \"managing\" a particular buffer, it
opinionatedly sets some peripheral Emacs facilites.
Xref and Company.  These overriding settings help ensure
consistent Nox behaviour and only stay in place until
\"managing\" stops (usually via `nox-shutdown'), whereupon the
previous settings are restored.

However, if you wish for Nox to stay out of a particular Emacs
facility that you'd like to keep control of, add a string, a
symbol, or a regexp here that will be matched against the
variable's name, and Nox will refrain from setting it.

For example, to keep your Company customization use

(add-to-list 'nox-stay-out-of 'company)")

(defun nox--stay-out-of-p (symbol)
  "Tell if NOX should stay of of SYMBOL."
  (cl-find (symbol-name symbol) nox-stay-out-of
           :test (lambda (s thing)
                   (let ((re (if (symbolp thing) (symbol-name thing) thing)))
                     (string-match re s)))))

(defmacro nox--setq-saving (symbol binding)
  `(unless (or (not (boundp ',symbol)) (nox--stay-out-of-p ',symbol))
     (push (cons ',symbol (symbol-value ',symbol)) nox--saved-bindings)
     (setq-local ,symbol ,binding)))

(defvar-local nox--cached-server nil
  "A cached reference to the current NOX server.")

(defun nox-managed-p ()
  "Tell if current buffer is managed by NOX."
  nox--managed-mode)

(make-obsolete-variable
 'nox--managed-mode-hook 'nox-managed-mode-hook "1.6")

(defvar nox-managed-mode-hook nil
  "A hook run by NOX after it started/stopped managing a buffer.
Use `nox-managed-p' to determine if current buffer is managed.")

(define-minor-mode nox--managed-mode
  "Mode for source buffers managed by some NOX project."
  nil nil nox-mode-map
  (cond
   (nox--managed-mode
    (add-hook 'after-change-functions 'nox--after-change nil t)
    (add-hook 'before-change-functions 'nox--before-change nil t)
    (add-hook 'kill-buffer-hook #'nox--managed-mode-off nil t)
    ;; Prepend "didClose" to the hook after the "onoff", so it will run first
    (add-hook 'kill-buffer-hook 'nox--signal-textDocument/didClose nil t)
    (add-hook 'before-revert-hook 'nox--signal-textDocument/didClose nil t)
    (add-hook 'after-revert-hook 'nox--after-revert-hook nil t)
    (add-hook 'before-save-hook 'nox--signal-textDocument/willSave nil t)
    (add-hook 'after-save-hook 'nox--signal-textDocument/didSave nil t)
    (add-hook 'xref-backend-functions 'nox-xref-backend nil t)
    (add-hook 'completion-at-point-functions #'nox-completion-at-point nil t)
    (add-hook 'change-major-mode-hook #'nox--managed-mode-off nil t)
    (add-hook 'post-self-insert-hook 'nox--post-self-insert-hook nil t)
    (add-hook 'pre-command-hook 'nox--pre-command-hook nil t)
    (add-hook 'post-command-hook 'nox-monitor-cursor-change 'append nil)
    (nox--setq-saving xref-prompt-for-identifier nil)
    (nox--setq-saving company-backends '(company-capf))
    (nox--setq-saving company-tooltip-align-annotations t)
    (cl-pushnew (current-buffer) (nox--managed-buffers nox--cached-server)))
   (t
    (remove-hook 'after-change-functions 'nox--after-change t)
    (remove-hook 'before-change-functions 'nox--before-change t)
    (remove-hook 'kill-buffer-hook #'nox--managed-mode-off t)
    (remove-hook 'kill-buffer-hook 'nox--signal-textDocument/didClose t)
    (remove-hook 'before-revert-hook 'nox--signal-textDocument/didClose t)
    (remove-hook 'after-revert-hook 'nox--after-revert-hook t)
    (remove-hook 'before-save-hook 'nox--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook 'nox--signal-textDocument/didSave t)
    (remove-hook 'xref-backend-functions 'nox-xref-backend t)
    (remove-hook 'completion-at-point-functions #'nox-completion-at-point t)
    (remove-hook 'change-major-mode-hook #'nox--managed-mode-off t)
    (remove-hook 'post-self-insert-hook 'nox--post-self-insert-hook t)
    (remove-hook 'pre-command-hook 'nox--pre-command-hook t)
    (remove-hook 'post-command-hook 'nox-monitor-cursor-change nil)
    (cl-loop for (var . saved-binding) in nox--saved-bindings
             do (set (make-local-variable var) saved-binding))
    (let ((server nox--cached-server))
      (setq nox--cached-server nil)
      (when server
        (setf (nox--managed-buffers server)
              (delq (current-buffer) (nox--managed-buffers server)))
        (when (and nox-autoshutdown
                   (null (nox--managed-buffers server)))
          (nox-shutdown server))))))
  ;; Note: the public hook runs before the internal nox--managed-mode-hook.
  (run-hooks 'nox-managed-mode-hook))

(defun nox--managed-mode-off ()
  "Turn off `nox--managed-mode' unconditionally."
  (nox--managed-mode -1))

(defun nox-current-server ()
  "Return logical NOX server for current buffer, nil if none."
  nox--cached-server)

(defun nox--current-server-or-lose ()
  "Return current logical NOX server connection or error."
  (or nox--cached-server
      (jsonrpc-error "No current JSON-RPC connection")))

(defvar-local nox--unreported-diagnostics nil
  "Unreported Flymake diagnostics for this buffer.")

(defvar revert-buffer-preserve-modes)
(defun nox--after-revert-hook ()
  "Nox's `after-revert-hook'."
  (when revert-buffer-preserve-modes (nox--signal-textDocument/didOpen)))

(defun nox--maybe-activate-editing-mode ()
  "Maybe activate `nox--managed-mode'.

If it is activated, also signal textDocument/didOpen."
  (unless nox--managed-mode
    ;; Called when `revert-buffer-in-progress-p' is t but
    ;; `revert-buffer-preserve-modes' is nil.
    (when (and buffer-file-name
               (or
                nox--cached-server
                (setq nox--cached-server
                      (cl-find major-mode
                               (gethash (or (project-current)
                                            `(transient . ,default-directory))
                                        nox--servers-by-project)
                               :key #'nox--major-mode))))
      (setq nox--unreported-diagnostics `(:just-opened . nil))
      (nox--managed-mode)
      (nox--signal-textDocument/didOpen))))

(add-hook 'find-file-hook 'nox--maybe-activate-editing-mode)
(add-hook 'after-change-major-mode-hook 'nox--maybe-activate-editing-mode)

(defun nox-clear-status (server)
  "Clear the last JSONRPC error for SERVER."
  (interactive (list (nox--current-server-or-lose)))
  (setf (jsonrpc-last-error server) nil))

;;; Protocol implementation (Requests, notifications, etc)
;;;
(cl-defmethod nox-handle-notification
  (_server method &key &allow-other-keys)
  "Handle unknown notification"
  (unless (or (string-prefix-p "$" (format "%s" method))
              (not (memq 'disallow-unknown-methods nox-strict-mode)))
    (nox--warn "Server sent unknown notification method `%s'" method)))

(cl-defmethod nox-handle-request
  (_server method &key &allow-other-keys)
  "Handle unknown request"
  (when (memq 'disallow-unknown-methods nox-strict-mode)
    (jsonrpc-error "Unknown request method `%s'" method)))

(cl-defmethod nox-execute-command
  (server command arguments)
  "Execute COMMAND on SERVER with `:workspace/executeCommand'.
COMMAND is a symbol naming the command."
  (jsonrpc-request server :workspace/executeCommand
                   `(:command ,(format "%s" command) :arguments ,arguments)))

(cl-defmethod nox-handle-notification
  (_server (_method (eql window/showMessage)) &key type message)
  "Handle notification window/showMessage"
  (nox--message (propertize "Server reports (type=%s): %s" 'face (if (<= type 1) 'error)) type message))

(cl-defmethod nox-handle-request
  (_server (_method (eql window/showMessageRequest)) &key type message actions)
  "Handle server request window/showMessageRequest"
  (let ((label (completing-read
                (concat
                 (format (propertize "[nox] Server reports (type=%s): %s"
                                     'face (if (<= type 1) 'error))
                         type message)
                 "\nChoose an option: ")
                (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
                    '("OK"))
                nil t (plist-get (elt actions 0) :title))))
    (if label `(:title ,label) :null)))

(cl-defmethod nox-handle-notification
  (_server (_method (eql window/logMessage)) &key _type _message)
  "Handle notification window/logMessage") ;; noop, use events buffer

(cl-defmethod nox-handle-notification
  (_server (_method (eql telemetry/event)) &rest _any)
  "Handle notification telemetry/event") ;; noop, use events buffer

(cl-defmethod nox-handle-notification
  (server (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics
          &allow-other-keys) ; FIXME: doesn't respect `nox-strict-mode'
  "Handle notification publishDiagnostics")

(cl-defun nox--register-unregister (server things how)
  "Helper for `registerCapability'.
THINGS are either registrations or unregisterations (sic)."
  (cl-loop
   for thing in (cl-coerce things 'list)
   do (nox--dbind ((Registration) id method registerOptions) thing
        (apply (cl-ecase how
                 (register 'nox-register-capability)
                 (unregister 'nox-unregister-capability))
               server (intern method) id registerOptions))))

(cl-defmethod nox-handle-request
  (server (_method (eql client/registerCapability)) &key registrations)
  "Handle server request client/registerCapability"
  (nox--register-unregister server registrations 'register))

(cl-defmethod nox-handle-request
  (server (_method (eql client/unregisterCapability))
          &key unregisterations) ;; XXX: "unregisterations" (sic)
  "Handle server request client/unregisterCapability"
  (nox--register-unregister server unregisterations 'unregister))

(cl-defmethod nox-handle-request
  (_server (_method (eql workspace/applyEdit)) &key _label edit)
  "Handle server request workspace/applyEdit"
  (nox--apply-workspace-edit edit nox-confirm-server-initiated-edits))

(defun nox--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  `(:uri ,(nox--path-to-uri (or buffer-file-name
                                (ignore-errors
                                  (buffer-file-name
                                   (buffer-base-buffer)))))))

(defvar-local nox--versioned-identifier 0)

(defun nox--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (nox--TextDocumentIdentifier)
          `(:version ,nox--versioned-identifier)))

(defun nox--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (nox--VersionedTextDocumentIdentifier)
   (list :languageId
         (if (string-match "\\(.*\\)-mode" (symbol-name major-mode))
             (match-string 1 (symbol-name major-mode))
           "unknown")
         :text
         (nox--widening
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun nox--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (list :textDocument (nox--TextDocumentIdentifier)
        :position (nox--pos-to-lsp-position)))

(defvar-local nox--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defun nox--post-self-insert-hook ()
  "Set `nox--last-inserted-char'."
  (setq nox--last-inserted-char last-input-event))

(defun nox--pre-command-hook ()
  "Reset `nox--last-inserted-char'."
  (setq nox--last-inserted-char nil))

(defun nox--CompletionParams ()
  (append
   (nox--TextDocumentPositionParams)
   `(:context
     ,(if-let (trigger (and (characterp nox--last-inserted-char)
                            (cl-find nox--last-inserted-char
                                     (nox--server-capable :completionProvider
                                                          :triggerCharacters)
                                     :key (lambda (str) (aref str 0))
                                     :test #'char-equal)))
          `(:triggerKind 2 :triggerCharacter ,trigger) `(:triggerKind 1)))))

(defvar-local nox--recent-changes nil
  "Recent buffer changes as collected by `nox--before-change'.")

(cl-defmethod jsonrpc-connection-ready-p ((_server nox-lsp-server) _what)
  "Tell if SERVER is ready for WHAT in current buffer."
  (and (cl-call-next-method) (not nox--recent-changes)))

(defvar-local nox--change-idle-timer nil "Idle timer for didChange signals.")

(defun nox--before-change (beg end)
  "Hook onto `before-change-functions' with BEG and END."
  (when (listp nox--recent-changes)
    ;; Records BEG and END, crucially convert them into LSP
    ;; (line/char) positions before that information is lost (because
    ;; the after-change thingy doesn't know if newlines were
    ;; deleted/added).  Also record markers of BEG and END
    ;; (github#259)
    (push `(,(nox--pos-to-lsp-position beg)
            ,(nox--pos-to-lsp-position end)
            (,beg . ,(copy-marker beg))
            (,end . ,(copy-marker end)))
          nox--recent-changes)))

(defun nox--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf nox--versioned-identifier)
  (pcase (and (listp nox--recent-changes)
              (car nox--recent-changes))
    (`(,lsp-beg ,lsp-end
                (,b-beg . ,b-beg-marker)
                (,b-end . ,b-end-marker))
     ;; github#259: With `upcase-word' or somesuch,
     ;; `before-change-functions' always records the whole word's
     ;; `beg' and `end'.  Not only is this longer than needed but
     ;; conflicts with the args received here.  Detect this using
     ;; markers recorded earlier and `pre-change-len', then fix it.
     (when (and (= b-end b-end-marker) (= b-beg b-beg-marker)
                (not (zerop pre-change-length)))
       (setq lsp-end (nox--pos-to-lsp-position end)
             lsp-beg (nox--pos-to-lsp-position beg)))
     (setcar nox--recent-changes
             `(,lsp-beg ,lsp-end ,pre-change-length
                        ,(buffer-substring-no-properties beg end))))
    (_ (setf nox--recent-changes :emacs-messup)))
  (when nox--change-idle-timer (cancel-timer nox--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq nox--change-idle-timer
          (run-with-idle-timer
           nox-send-changes-idle-time
           nil (lambda () (nox--with-live-buffer buf
                        (when nox--managed-mode
                          (nox--signal-textDocument/didChange)
                          (setq nox--change-idle-timer nil))))))))

;; HACK! Launching a deferred sync request with outstanding changes is a
;; bad idea, since that might lead to the request never having a
;; chance to run, because `jsonrpc-connection-ready-p'.
(advice-add #'jsonrpc-request :before
            (cl-function (lambda (_proc _method _params &key
                                    deferred &allow-other-keys)
                           (when (and nox--managed-mode deferred)
                             (nox--signal-textDocument/didChange))))
            '((name . nox--signal-textDocument/didChange)))

(defvar-local nox-workspace-configuration ()
  "Alist of (SECTION . VALUE) entries configuring the LSP server.
SECTION should be a keyword or a string, value can be anything
that can be converted to JSON.")

(put 'nox-workspace-configuration 'safe-local-variable 'listp)

(defun nox-signal-didChangeConfiguration (server)
  "Send a `:workspace/didChangeConfiguration' signal to SERVER.
When called interactively, use the currently active server"
  (interactive (list (nox--current-server-or-lose)))
  (jsonrpc-notify
   server :workspace/didChangeConfiguration
   (list
    :settings
    (cl-loop for (section . v) in nox-workspace-configuration
             collect (if (keywordp section)
                         section
                       (intern (format ":%s" section)))
             collect v))))

(cl-defmethod nox-handle-request
  (server (_method (eql workspace/configuration)) &key items)
  "Handle server request workspace/configuration."
  (apply #'vector
         (mapcar
          (nox--lambda ((ConfigurationItem) scopeUri section)
            (with-temp-buffer
              (let* ((uri-path (nox--uri-to-path scopeUri))
                     (default-directory
                       (if (and (not (string-empty-p uri-path))
                                (file-directory-p uri-path))
                           uri-path
                         (car (project-roots (nox--project server))))))
                (setq-local major-mode (nox--major-mode server))
                (hack-dir-local-variables-non-file-buffer)
                (alist-get section nox-workspace-configuration
                           nil nil
                           (lambda (wsection section)
                             (string=
                              (if (keywordp wsection)
                                  (substring (symbol-name wsection) 1)
                                wsection)
                              section))))))
          items)))

(defun nox--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when nox--recent-changes
    (let* ((server (nox--current-server-or-lose))
           (sync-capability (nox--server-capable :textDocumentSync))
           (sync-kind (if (numberp sync-capability) sync-capability
                        (plist-get sync-capability :change)))
           (full-sync-p (or (eq sync-kind 1)
                            (eq :emacs-messup nox--recent-changes))))
      (jsonrpc-notify
       server :textDocument/didChange
       (list
        :textDocument (nox--VersionedTextDocumentIdentifier)
        :contentChanges
        (if full-sync-p
            (vector `(:text ,(nox--widening
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
          (cl-loop for (beg end len text) in (reverse nox--recent-changes)
                   ;; github#259: `capitalize-word' and commands based
                   ;; on `casify_region' will cause multiple duplicate
                   ;; empty entries in `nox--before-change' calls
                   ;; without an `nox--after-change' reciprocal.
                   ;; Weed them out here.
                   when (numberp len)
                   vconcat `[,(list :range `(:start ,beg :end ,end)
                                    :rangeLength len :text text)]))))
      (setq nox--recent-changes nil)
      (setf (nox--spinner server) (list nil :textDocument/didChange t))
      (jsonrpc--call-deferred server))))

(defun nox--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq nox--recent-changes nil nox--versioned-identifier 0)
  (jsonrpc-notify
   (nox--current-server-or-lose)
   :textDocument/didOpen `(:textDocument ,(nox--TextDocumentItem))))

(defun nox--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[nox] error sending textDocument/didClose: %s"
    (jsonrpc-notify
     (nox--current-server-or-lose)
     :textDocument/didClose `(:textDocument ,(nox--TextDocumentIdentifier)))))

(defun nox--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (let ((server (nox--current-server-or-lose))
        (params `(:reason 1 :textDocument ,(nox--TextDocumentIdentifier))))
    (jsonrpc-notify server :textDocument/willSave params)
    (when (nox--server-capable :textDocumentSync :willSaveWaitUntil)
      (ignore-errors
        (nox--apply-text-edits
         (jsonrpc-request server :textDocument/willSaveWaitUntil params
                          :timeout 0.5))))))

(defun nox--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (nox--signal-textDocument/didChange)
  (jsonrpc-notify
   (nox--current-server-or-lose)
   :textDocument/didSave
   (list
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (nox--TextDocumentIdentifier))))

(defun nox-xref-backend () "NOX xref backend." 'nox)

(defvar nox--temp-location-buffers (make-hash-table :test #'equal)
  "Helper variable for `nox--handling-xrefs'.")

(defvar nox-xref-lessp-function #'ignore
  "Compare two `xref-item' objects for sorting.")

(cl-defmacro nox--collecting-xrefs ((collector) &rest body)
  "Sort and handle xrefs collected with COLLECTOR in BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((collected (cl-gensym "collected")))
    `(unwind-protect
         (let (,collected)
           (cl-flet ((,collector (xref) (push xref ,collected)))
             ,@body)
           (sort ,collected nox-xref-lessp-function))
       (maphash (lambda (_uri buf) (kill-buffer buf)) nox--temp-location-buffers)
       (clrhash nox--temp-location-buffers))))

(defun nox--xref-make (name uri range)
  "Like `xref-make' but with LSP's NAME, URI and RANGE.
Try to visit the target file for a richer summary line."
  (pcase-let*
      ((file (nox--uri-to-path uri))
       (visiting (or (find-buffer-visiting file)
                     (gethash uri nox--temp-location-buffers)))
       (collect (lambda ()
                  (nox--widening
                   (pcase-let* ((`(,beg . ,end) (nox--range-region range))
                                (bol (progn (goto-char beg) (point-at-bol)))
                                (substring (buffer-substring bol (point-at-eol)))
                                (hi-beg (- beg bol))
                                (hi-end (- (min (point-at-eol) end) bol)))
                     (add-face-text-property hi-beg hi-end 'highlight
                                             t substring)
                     (list substring (1+ (current-line)) (nox-current-column))))))
       (`(,summary ,line ,column)
        (cond
         (visiting (with-current-buffer visiting (funcall collect)))
         ((file-readable-p file) (with-current-buffer
                                     (puthash uri (generate-new-buffer " *temp*")
                                              nox--temp-location-buffers)
                                   (insert-file-contents file)
                                   (funcall collect)))
         (t ;; fall back to the "dumb strategy"
          (let ((start (cl-getf range :start)))
            (list name (1+ (cl-getf start :line)) (cl-getf start :character)))))))
    (xref-make summary (xref-make-file-location file line column))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql nox)))
  (nox--error "cannot (yet) provide reliable completion table for LSP symbols"))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql nox)))
  ;; JT@19/10/09: This is a totally dummy identifier that isn't even
  ;; passed to LSP.  The reason for this particular wording is to
  ;; construct a readable message "No references for LSP identifier at
  ;; point.".   See http://github.com/joaotavora/nox/issues/314
  "LSP identifier at point.")

(defvar nox--lsp-xref-refs nil
  "`xref' objects for overriding `xref-backend-references''s.")

(cl-defun nox--lsp-xrefs-for-method (method &key extra-params capability)
  "Make `xref''s for METHOD, EXTRA-PARAMS, check CAPABILITY."
  (unless (nox--server-capable
           (or capability
               (intern
                (format ":%sProvider"
                        (cadr (split-string (symbol-name method)
                                            "/"))))))
    (nox--error "Sorry, this server doesn't do %s" method))
  (let ((response
         (jsonrpc-request
          (nox--current-server-or-lose)
          method (append (nox--TextDocumentPositionParams) extra-params))))
    (nox--collecting-xrefs (collect)
      (mapc
       (nox--lambda ((Location) uri range)
         (collect (nox--xref-make (symbol-at-point) uri range)))
       (if (vectorp response) response (list response))))))

(cl-defun nox--lsp-xref-helper (method &key extra-params capability )
  "Helper for `nox-find-declaration' & friends."
  (let ((nox--lsp-xref-refs (nox--lsp-xrefs-for-method
                             method
                             :extra-params extra-params
                             :capability capability)))
    (if nox--lsp-xref-refs
        (xref-find-references "LSP identifier at point.")
      (nox--message "%s returned no references" method))))

(defun nox-find-declaration ()
  "Find declaration for SYM, the identifier at point."
  (interactive)
  (nox--lsp-xref-helper :textDocument/declaration))

(defun nox-find-implementation ()
  "Find implementation for SYM, the identifier at point."
  (interactive)
  (nox--lsp-xref-helper :textDocument/implementation))

(defun nox-find-typeDefinition ()
  "Find type definition for SYM, the identifier at point."
  (interactive)
  (nox--lsp-xref-helper :textDocument/typeDefinition))

(cl-defmethod xref-backend-definitions ((_backend (eql nox)) _identifier)
  (nox--lsp-xrefs-for-method :textDocument/definition))

(cl-defmethod xref-backend-references ((_backend (eql nox)) _identifier)
  (or
   nox--lsp-xref-refs
   (nox--lsp-xrefs-for-method
    :textDocument/references :extra-params `(:context (:includeDeclaration t)))))

(cl-defmethod xref-backend-apropos ((_backend (eql nox)) pattern)
  (when (nox--server-capable :workspaceSymbolProvider)
    (nox--collecting-xrefs (collect)
      (mapc
       (nox--lambda ((SymbolInformation) name location)
         (nox--dbind ((Location) uri range) location
           (collect (nox--xref-make name uri range))))
       (jsonrpc-request (nox--current-server-or-lose)
                        :workspace/symbol
                        `(:query ,pattern))))))

(defun nox-format-buffer ()
  "Format contents of current buffer."
  (interactive)
  (nox-format nil nil))

(defun nox-format (&optional beg end)
  "Format region BEG END.
If either BEG or END is nil, format entire buffer.
Interactively, format active region, or entire buffer if region
is not active."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (pcase-let ((`(,method ,cap ,args)
               (cond
                ((and beg end)
                 `(:textDocument/rangeFormatting
                   :documentRangeFormattingProvider
                   (:range ,(list :start (nox--pos-to-lsp-position beg)
                                  :end (nox--pos-to-lsp-position end)))))
                (t
                 '(:textDocument/formatting :documentFormattingProvider nil)))))
    (unless (nox--server-capable cap)
      (nox--error "Server can't format!"))
    (nox--apply-text-edits
     (jsonrpc-request
      (nox--current-server-or-lose)
      method
      (cl-list*
       :textDocument (nox--TextDocumentIdentifier)
       :options (list :tabSize tab-width
                      :insertSpaces (if indent-tabs-mode :json-false t))
       args)
      :deferred method))))

(defun nox-completion-at-point ()
  "NOX's `completion-at-point' function."
  ;; Commit logs for this function help understand what's going on.
  (when-let (completion-capability (nox--server-capable :completionProvider))
    (let* ((server (nox--current-server-or-lose))
           (sort-completions (lambda (completions)
                               (sort completions
                                     (lambda (a b)
                                       (string-lessp
                                        (or (get-text-property 0 :sortText a) "")
                                        (or (get-text-property 0 :sortText b) ""))))))
           (metadata `(metadata . ((display-sort-function . ,sort-completions))))
           resp
           items
           (cached-proxies :none)
           (proxies
            (lambda ()
              (if (listp cached-proxies) cached-proxies
                (setq resp
                      (jsonrpc-request server
                                       :textDocument/completion
                                       (nox--CompletionParams)
                                       :deferred :textDocument/completion
                                       :cancel-on-input t))
                (setq items (append
                             (if (vectorp resp) resp (plist-get resp :items))
                             nil))
                (setq cached-proxies
                      (mapcar
                       (jsonrpc-lambda
                           (&rest item &key label kind insertText filterText
                                  &allow-other-keys)
                         (let ((proxy (nox--build-candidate-text kind label insertText filterText)))
                           (unless (zerop (length item))
                             (put-text-property 0 1 'nox--lsp-item item proxy))
                           proxy))
                       items)))))
           resolved
           (resolve-maybe
            ;; Maybe completion/resolve JSON object `lsp-comp' into
            ;; another JSON object, if at all possible.  Otherwise,
            ;; just return lsp-comp.
            (lambda (lsp-comp)
              (cond (resolved resolved)
                    ((and (nox--server-capable :completionProvider
                                               :resolveProvider)
                          (plist-get lsp-comp :data))
                     (setq resolved
                           (jsonrpc-request server :completionItem/resolve
                                            lsp-comp :cancel-on-input t)))
                    (t lsp-comp))))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (lambda (probe pred action)
         (cond
          ((eq action 'metadata) metadata) ; metadata
          ((eq action 'lambda)                 ; test-completion
           (member probe (funcall proxies)))
          ((eq (car-safe action) 'boundaries) nil) ; boundaries
          ((and (null action)                      ; try-completion
                (member probe (funcall proxies)) t))
          ((eq action t)                ; all-completions
           (cl-remove-if-not
            (lambda (proxy)
              (let* ((item (get-text-property 0 'nox--lsp-item proxy))
                     (filterText (plist-get item :filterText)))
                (and (or (null pred) (funcall pred proxy))
                     (string-prefix-p
                      probe (or filterText proxy) completion-ignore-case))))
            (funcall proxies)))))
       :annotation-function
       (lambda (proxy)
         (nox--dbind ((CompletionItem) label kind)
             (get-text-property 0 'nox--lsp-item proxy)
           (let* ((label (and (stringp label)
                              (not (string= label ""))
                              label))
                  (annotation label)
                  (annotation-length (length annotation))
                  (kind-name (cdr (assoc kind nox--kind-names))))
             (when annotation
               (format " [%s] %s"
                       kind-name
                       (propertize
                        (if (> annotation-length nox-candidate-annotation-limit)
                            (concat (substring annotation 0 nox-candidate-annotation-limit) " ...")
                          annotation)
                        'face 'font-lock-function-name-face))))))
       :company-doc-buffer
       (lambda (proxy)
         (let* ((documentation
                 (let ((lsp-comp (get-text-property 0 'nox--lsp-item proxy)))
                   (plist-get (funcall resolve-maybe lsp-comp) :documentation)))
                (formatted (and documentation
                                (nox--format-markup documentation))))
           (when formatted
             (with-current-buffer (get-buffer-create " *nox doc*")
               (erase-buffer)
               (insert formatted)
               (current-buffer)))))
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (when (car bounds) (goto-char (car bounds)))
         (when (listp completion-capability)
           (looking-back
            (regexp-opt
             (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
            (line-beginning-position))))
       :exit-function
       (lambda (proxy _status)
         ;; Just send didChange request to server after finish completion.
         (nox--signal-textDocument/didChange)
         )))))

(defun nox--build-candidate-text (kind label insertText filterText)
  (cond
   ;; In C++, we will use filterText instead label, avoid too long candidate
   ((and filterText
         (< (length filterText) (length label)))
    (string-trim-left filterText))
   ((and insertText
         (not (string-empty-p insertText)))
    insertText)
   (t
    (string-trim-left label))))

(defun nox--hover-info (contents &optional range)
  (let ((heading (and range (pcase-let ((`(,beg . ,end) (nox--range-region range)))
                              (concat (buffer-substring beg end)  ": "))))
        (body (mapconcat #'nox--format-markup
                         (if (vectorp contents) contents (list contents)) "\n")))
    (when (or heading (cl-plusp (length body))) (concat heading body))))

(defun nox--sig-info (sigs active-sig active-param)
  (cl-loop
   for (sig . moresigs) on (append sigs nil) for i from 0
   concat
   (nox--dbind ((SignatureInformation) label documentation parameters) sig
     (with-temp-buffer
       (save-excursion (insert label))
       (let (params-start params-end)
         ;; Ad-hoc attempt to parse label as <name>(<params>)
         (when (looking-at "\\([^(]+\\)(\\([^)]+\\))")
           (setq params-start (match-beginning 2) params-end (match-end 2))
           (add-face-text-property (match-beginning 1) (match-end 1)
                                   'font-lock-function-name-face))
         (when (eql i active-sig)
           ;; Decide whether to add one-line-summary to signature line
           (when (and (stringp documentation)
                      (string-match "[[:space:]]*\\([^.\r\n]+[.]?\\)"
                                    documentation))
             (setq documentation (match-string 1 documentation))
             (unless (string-prefix-p (string-trim documentation) label)
               (goto-char (point-max))
               (insert ": " (nox--format-markup documentation))))
           ;; Decide what to do with the active parameter...
           (when (and (eql i active-sig) active-param
                      (< -1 active-param (length parameters)))
             (nox--dbind ((ParameterInformation) label documentation)
                 (aref parameters active-param)
               ;; ...perhaps highlight it in the formals list
               (when params-start
                 (goto-char params-start)
                 (pcase-let
                     ((`(,beg ,end)
                       (if (stringp label)
                           (let ((case-fold-search nil))
                             (and (re-search-forward
                                   (concat "\\<" (regexp-quote label) "\\>")
                                   params-end t)
                                  (list (match-beginning 0) (match-end 0))))
                         (mapcar #'1+ (append label nil)))))
                   (if (and beg end)
                       (add-face-text-property beg end 'highlight))))
               ;; ...and/or maybe add its doc on a line by its own.
               (when documentation
                 (goto-char (point-max))
                 (insert "\n"
                         (propertize
                          (if (stringp label)
                              label
                            (apply #'buffer-substring (mapcar #'1+ label)))
                          'face 'highlight)
                         ": " (nox--format-markup documentation))))))
         (buffer-string))))
   when moresigs concat "\n"))

(defun nox-color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (apply #'(lambda (r g b)
             (format "#%02x%02x%02x"
                     (ash r -8)
                     (ash g -8)
                     (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun nox--show-doc (string)
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (background-color
          (cond ((eq bg-mode 'dark)
                 (nox-color-blend (face-background 'default) "#000000" 0.5))
                ((eq bg-mode 'light)
                 (nox-color-blend (face-background 'default) "#000000" 0.9)))))
    (posframe-show
     nox-doc-tooltip-name
     :string string
     :font (format "%s-%s" (frame-parameter nil 'font-parameter) nox-doc-tooltip-font-size)
     :position (point)
     :timeout nox-doc-tooltip-timeout
     :background-color background-color
     :foreground-color (face-attribute 'default :foreground)
     :internal-border-width nox-doc-tooltip-border-width)))

(defun nox-show-doc ()
  "Show documentation at point, use by `posframe'."
  (interactive)
  (let* ((buffer (current-buffer))
         (server (nox--current-server-or-lose))
         (position-params (nox--TextDocumentPositionParams))
         sig-showing)
    (cl-macrolet ((when-buffer-window
                   (&body body) ; notice the exception when testing with `ert'
                   `(when (or (get-buffer-window buffer) (ert-running-test))
                      (with-current-buffer buffer ,@body))))
      (when (nox--server-capable :signatureHelpProvider)
        (jsonrpc-async-request
         server :textDocument/signatureHelp position-params
         :success-fn
         (nox--lambda ((SignatureHelp) signatures activeSignature activeParameter)
           (when-buffer-window
            (when (cl-plusp (length signatures))
              (setq sig-showing t)
              (nox--show-doc (nox--sig-info signatures activeSignature activeParameter)))))
         :deferred :textDocument/signatureHelp))
      (when (nox--server-capable :hoverProvider)
        (jsonrpc-async-request
         server :textDocument/hover position-params
         :success-fn (nox--lambda ((Hover) contents range)
                       (unless sig-showing
                         (when-buffer-window
                          (when-let (info (and (not (seq-empty-p contents))
                                               (nox--hover-info contents
                                                                range)))
                            (nox--show-doc info)))))
         :deferred :textDocument/hover)))))

(defvar nox-last-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(defun nox-monitor-cursor-change ()
  (unless (equal (point) nox-last-position)
    (posframe-hide nox-doc-tooltip-name))
  (setq nox-last-position (point)))

(defun nox--apply-text-edits (edits &optional version)
  "Apply EDITS for current buffer if at VERSION, or if it's nil."
  (unless (or (not version) (equal version nox--versioned-identifier))
    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version nox--versioned-identifier))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (make-progress-reporter
                      (format "[nox] applying %s edits to `%s'..."
                              howmany (current-buffer))
                      0 howmany))
           (done 0))
      (mapc (pcase-lambda (`(,newText ,beg . ,end))
              (let ((source (current-buffer)))
                (with-temp-buffer
                  (insert newText)
                  (let ((temp (current-buffer)))
                    (with-current-buffer source
                      (save-excursion
                        (save-restriction
                          (narrow-to-region beg end)

                          ;; On emacs versions < 26.2,
                          ;; `replace-buffer-contents' is buggy - it calls
                          ;; change functions with invalid arguments - so we
                          ;; manually call the change functions here.
                          ;;
                          ;; See emacs bugs #32237, #32278:
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
                          (let ((inhibit-modification-hooks t)
                                (length (- end beg))
                                (beg (marker-position beg))
                                (end (marker-position end)))
                            (run-hook-with-args 'before-change-functions
                                                beg end)
                            (replace-buffer-contents temp)
                            (run-hook-with-args 'after-change-functions
                                                beg (+ beg (length newText))
                                                length))))
                      (progress-reporter-update reporter (cl-incf done)))))))
            (mapcar (nox--lambda ((TextEdit) range newText)
                      (cons newText (nox--range-region range 'markers)))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (progress-reporter-done reporter))))

(defun nox--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (nox--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (if (or (and changes
                 (> (length changes) 0))
            (and documentChanges
                 (> (length documentChanges) 0)))
        (let ((prepared
               (mapcar (nox--lambda ((TextDocumentEdit) textDocument edits)
                         (nox--dbind ((VersionedTextDocumentIdentifier) uri version)
                             textDocument
                           (list (nox--uri-to-path uri) edits version)))
                       documentChanges))
              edit)
          (cl-loop for (uri edits) on changes by #'cddr
                   do (push (list (nox--uri-to-path uri) edits) prepared))
          (if (or confirm
                  (cl-notevery #'find-buffer-visiting
                               (mapcar #'car prepared)))
              (unless (y-or-n-p
                       (format "[nox] Server wants to edit (y or n): \n  %s\n Proceed? "
                               (mapconcat #'identity (mapcar #'car prepared) "\n  ")))
                (nox--error "User cancelled server edit")))
          (while (setq edit (car prepared))
            (pcase-let ((`(,path ,edits ,version)  edit))
              (with-current-buffer (find-file-noselect path)
                (nox--apply-text-edits edits version))
              (pop prepared))
            t)
          (unwind-protect
              (if prepared (nox--warn "Caution: edits of files %s failed."
                                      (mapcar #'car prepared))
                (nox--message "Edit successful!"))))
      ;; If respond arguments is nil, something wrong in LSP server.
      (nox--message "Nothing change, please execute command `nox-stderr-buffer` to check reason!"))))

(defun nox-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (nox--server-capable :renameProvider)
    (nox--error "Server can't rename!"))
  (nox--apply-workspace-edit
   (jsonrpc-request (nox--current-server-or-lose)
                    :textDocument/rename `(,@(nox--TextDocumentPositionParams)
                                           :newName ,newname))
   current-prefix-arg))


;;; Dynamic registration
;;;
(defun nox--wildcard-to-regexp (wildcard)
  "(Very lame attempt to) convert WILDCARD to a Elisp regexp."
  (cl-loop
   with substs = '(("{" . "\\\\(")
                   ("}" . "\\\\)")
                   ("," . "\\\\|"))
   with string = (wildcard-to-regexp wildcard)
   for (pattern . rep) in substs
   for target = string then result
   for result = (replace-regexp-in-string pattern rep target)
   finally return result))

(cl-defmethod nox-register-capability
  (server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles"
  (nox-unregister-capability server method id)
  (let* (success
         (globs (mapcar (nox--lambda ((FileSystemWatcher) globPattern)
                          globPattern)
                        watchers))
         (glob-dirs
          (delete-dups (mapcar #'file-name-directory
                               (mapcan #'file-expand-wildcards globs)))))
    (cl-labels
        ((handle-event
          (event)
          (pcase-let ((`(,desc ,action ,file ,file1) event))
            (cond
             ((and (memq action '(created changed deleted))
                   (cl-find file globs
                            :test (lambda (f glob)
                                    (string-match (nox--wildcard-to-regexp
                                                   (expand-file-name glob))
                                                  f))))
              (jsonrpc-notify
               server :workspace/didChangeWatchedFiles
               `(:changes ,(vector `(:uri ,(nox--path-to-uri file)
                                          :type ,(cl-case action
                                                   (created 1)
                                                   (changed 2)
                                                   (deleted 3)))))))
             ((eq action 'renamed)
              (handle-event `(,desc 'deleted ,file))
              (handle-event `(,desc 'created ,file1)))))))
      (unwind-protect
          (progn
            (dolist (dir glob-dirs)
              (push (file-notify-add-watch dir '(change) #'handle-event)
                    (gethash id (nox--file-watches server))))
            (setq
             success
             `(:message ,(format "OK, watching %s directories in %s watchers"
                                 (length glob-dirs) (length watchers)))))
        (unless success
          (nox-unregister-capability server method id))))))

(cl-defmethod nox-unregister-capability
  (server (_method (eql workspace/didChangeWatchedFiles)) id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles"
  (mapc #'file-notify-rm-watch (gethash id (nox--file-watches server)))
  (remhash id (nox--file-watches server))
  (list t "OK"))

;;; Rust-specific
;;;
(defclass nox-rls (nox-lsp-server) () :documentation "Rustlang's RLS.")

(cl-defmethod jsonrpc-connection-ready-p ((server nox-rls) what)
  "Except for :completion, RLS isn't ready until Indexing done."
  (and (cl-call-next-method)
       (or ;; RLS normally ready for this, even if building.
        (eq :textDocument/completion what)
        (pcase-let ((`(,_id ,what ,done ,_detail) (nox--spinner server)))
          (and (equal "Indexing" what) done)))))

(cl-defmethod nox-handle-notification
  ((server nox-rls) (_method (eql window/progress))
   &key id done title message &allow-other-keys)
  "Handle notification window/progress"
  (setf (nox--spinner server) (list id title done message)))

;;; eclipse-jdt-specific
;;;
(defclass nox-eclipse-jdt (nox-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(cl-defmethod nox-initialization-options ((server nox-eclipse-jdt))
  "Passes through required jdt initialization options"
  `(:workspaceFolders
    [,@(cl-delete-duplicates
        (mapcar #'nox--path-to-uri
                (let* ((roots (project-roots (nox--project server)))
                       (root (car roots)))
                  (append
                   roots
                   (mapcar
                    #'file-name-directory
                    (append
                     (file-expand-wildcards (concat root "*/pom.xml"))
                     (file-expand-wildcards (concat root "*/build.gradle"))
                     (file-expand-wildcards (concat root "*/.project")))))))
        :test #'string=)]
    ,@(if-let ((home (or (getenv "JAVA_HOME")
                         (ignore-errors
                           (expand-file-name
                            ".."
                            (file-name-directory
                             (file-chase-links (executable-find "javac"))))))))
          `(:settings (:java (:home ,home)))
        (ignore (nox--warn "JAVA_HOME env var not set")))))

(defun nox--eclipse-jdt-contact (interactive)
  "Return a contact for connecting to eclipse.jdt.ls server, as a cons cell.
If INTERACTIVE, prompt user for details."
  (cl-labels
      ((is-the-jar
        (path)
        (and (string-match-p
              "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"
              (file-name-nondirectory path))
             (file-exists-p path))))
    (let* ((classpath (or (getenv "CLASSPATH") ":"))
           (cp-jar (cl-find-if #'is-the-jar (split-string classpath ":")))
           (jar cp-jar)
           (dir
            (cond
             (jar (file-name-as-directory
                   (expand-file-name ".." (file-name-directory jar))))
             (interactive
              (expand-file-name
               (read-directory-name
                (concat "Path to eclipse.jdt.ls directory (could not"
                        " find it in CLASSPATH): ")
                nil nil t)))
             (t (error "Could not find eclipse.jdt.ls jar in CLASSPATH"))))
           (repodir
            (concat dir
                    "org.eclipse.jdt.ls.product/target/repository/"))
           (repodir (if (file-directory-p repodir) repodir dir))
           (config
            (concat
             repodir
             (cond
              ((string= system-type "darwin") "config_mac")
              ((string= system-type "windows-nt") "config_win")
              (t "config_linux"))))
           (project (or (project-current) `(transient . ,default-directory)))
           (workspace
            (expand-file-name (md5 (car (project-roots project)))
                              (concat user-emacs-directory
                                      "nox-eclipse-jdt-cache"))))
      (unless jar
        (setq jar
              (cl-find-if #'is-the-jar
                          (directory-files (concat repodir "plugins") t))))
      (unless (and jar (file-exists-p jar) (file-directory-p config))
        (error "Could not find required eclipse.jdt.ls files (build required?)"))
      (when (and interactive (not cp-jar)
                 (y-or-n-p (concat "Add path to the server program "
                                   "to CLASSPATH environment variable?")))
        (setenv "CLASSPATH" (concat (getenv "CLASSPATH") ":" jar)))
      (unless (file-directory-p workspace)
        (make-directory workspace t))
      (cons 'nox-eclipse-jdt
            (list (executable-find "java")
                  "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                  "-Dosgi.bundles.defaultStartLevel=4"
                  "-Declipse.product=org.eclipse.jdt.ls.core.product"
                  "-jar" jar
                  "-configuration" config
                  "-data" workspace)))))

(cl-defmethod nox-execute-command
  ((_server nox-eclipse-jdt) (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'nox--apply-workspace-edit arguments))

(provide 'nox)
;;; nox.el ends here
