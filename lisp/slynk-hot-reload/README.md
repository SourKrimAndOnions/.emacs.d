# slynk-hot-reload

Hot reload system for Hunchentoot web applications with Sly/Emacs integration.

Automatically refreshes your browser when you compile code with `C-c C-c` or `C-c C-k` in Emacs.

## Features

- 🔄 Automatic browser refresh on code compilation
- 🚀 Works with both `C-c C-c` (compile-defun) and `C-c C-k` (compile-file)
- 📦 **Single installation** - Just an Emacs package, no Quicklisp dependency needed
- 🎯 Zero configuration for new projects (2 lines of code)
- 🧹 Clean, simple API
- 📡 Uses Server-Sent Events (SSE) for reliable communication
- 🔇 No REPL spam - silent operation

## Installation

### Emacs Package

1. Copy or clone this directory to `~/.emacs.d/lisp/slynk-hot-reload/`

2. Add to your `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/slynk-hot-reload")
(require 'slynk-hot-reload)
(slynk-hot-reload-mode 1)
```

That's it! The Common Lisp backend is bundled in the `slynk/` subdirectory and automatically loaded into your Lisp process.

## Usage

### In Your Common Lisp Project

**1. Setup hot reload when starting your server:**

```lisp
(defun start (&key (port 8080))
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *acceptor*)
  (slynk-hot-reload:setup :acceptor *acceptor*)  ; Add this line
  *acceptor*)
```

**2. Include the script in your HTML templates:**

```lisp
;; Using Spinneret:
(:script (:raw (slynk-hot-reload:script)))

;; Using cl-who:
(htm (:script (str (slynk-hot-reload:script))))

;; Raw HTML:
(format stream "<script>~A</script>" (slynk-hot-reload:script))
```

**That's it!** No `.asd` dependencies needed - the Emacs package handles loading the CL backend.

### Per-Project Configuration (Optional)

To automatically enable hot reload when opening a project, create `.dir-locals.el` in your project root:

```elisp
;;; Directory Local Variables
((nil . ((eval . (progn
                   (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/slynk-hot-reload"))
                   (require 'slynk-hot-reload)
                   (slynk-hot-reload-mode 1))))))
```

## API

### Common Lisp

- `(slynk-hot-reload:setup &key acceptor)` - Register the hot reload SSE endpoint
- `(slynk-hot-reload:script)` - Returns JavaScript code to include in HTML
- `(slynk-hot-reload:reload)` - Manually trigger a browser reload

### Configuration Variables

- `slynk-hot-reload:*dev-mode*` - When `T`, hot reload is enabled (default: `T`)
- `slynk-hot-reload:*endpoint-path*` - SSE endpoint path (default: `"/_dev/reload"`)

### Emacs

- `M-x slynk-hot-reload-mode` - Toggle hot reload globally
- `slynk-hot-reload-reload-function` - Customize which function to call (default: `'(slynk-hot-reload:reload)`)

## How It Works

1. When you enable `slynk-hot-reload-mode`, the Emacs package:
   - Registers the bundled Common Lisp backend with ASDF
   - Loads the `slynk-hot-reload` system into your Lisp process
   - Hooks into Sly's compilation events

2. When you compile code (`C-c C-c` or `C-c C-k`):
   - Emacs calls `(slynk-hot-reload:reload)` in your REPL
   - This increments a version counter and notifies browsers via SSE
   - Connected browsers receive the reload event and refresh

3. The JavaScript in your HTML maintains an SSE connection to `/_dev/reload`
4. Heartbeat pings every 2 seconds keep the connection alive

## Example Project Structure

```
my-web-app/
├── my-web-app.asd           # No hot reload dependency needed!
├── .dir-locals.el           # Optional: auto-enable per project
└── src/
    ├── package.lisp
    ├── server.lisp          # Call (slynk-hot-reload:setup)
    └── views.lisp           # Include (slynk-hot-reload:script)
```

## Disabling for Production

Set `slynk-hot-reload:*dev-mode*` to `NIL` before starting your server in production:

```lisp
(setf slynk-hot-reload:*dev-mode* nil)
(start-server)
```

When disabled, `(slynk-hot-reload:script)` returns an empty string, so no JavaScript is injected.

## Troubleshooting

**Browser not reloading?**

1. Check browser console (F12) - you should see "Hot reload ready: v0"
2. Check the Network tab - you should see an active SSE connection to `/_dev/reload`
3. Ensure `slynk-hot-reload-mode` is enabled (check mode line for "HotReload")
4. Try manually: `(slynk-hot-reload:reload)` in the REPL - browser should reload

**Connection errors?**

- Make sure your server is running and the hot reload endpoint is registered
- Check for firewall/proxy issues if running on a remote server

**Backend not loading?**

- Check *sly-events* buffer for errors
- Ensure the `slynk/` subdirectory exists in the package directory
- Try manually: `(asdf:load-system :slynk-hot-reload)` in the REPL

## MELPA Submission (Future)

This package follows MELPA guidelines and can be submitted:
- All files use proper headers and commentary sections
- Package metadata in `slynk-hot-reload-pkg.el`
- Non-.el files (CL backend) in `slynk/` subdirectory
- Auto-loading and auto-discovery built-in

## License

MIT

## Example

See the included example project: `~/Work/common-lisp/time-reg/`
