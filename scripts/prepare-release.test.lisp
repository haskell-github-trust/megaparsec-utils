#!/usr/bin/env -S sbcl --script

(require :uiop)

(defvar *run-main* t)

(let ((*run-main* nil))
  (load (uiop:merge-pathnames* "prepare-release.lisp"
                               (uiop:pathname-directory-pathname *load-pathname*))))

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (error "assertion failed:~%expected: ~s~%actual:   ~s" expected actual)))

(defun assert-true (value)
  (unless value
    (error "assertion failed: expected true, got ~s" value)))

(defun assert-false (value)
  (when value
    (error "assertion failed: expected false, got ~s" value)))

(let ((hoogle "@package megaparsec-utils 0.1.1
module Text.Megaparsec.Utils
type LoggerIO = Loc -> IO ()
runLoggerWith :: IOE :> es => LoggerIO -> Eff (Logger : es) a -> Eff es a
instance MonadLogger (Eff es)
[LoggerLog] :: Loc -> LogSource -> LogLevel -> LogStr -> Logger m ()
class MonadLogger m where {
  monadLoggerLog :: m ()
}
"))
  (assert-equal '("Text.Megaparsec.Utils.LoggerIO"
                  "Text.Megaparsec.Utils.MonadLogger"
                  "Text.Megaparsec.Utils.MonadLogger (Eff es)"
                  "Text.Megaparsec.Utils.runLoggerWith")
                (parse-hoogle hoogle)))

(assert-equal "-- | Runs the logger.
-- @since 0.1.1
runLoggerWith :: LoggerIO -> a
"
              (add-since "-- | Runs the logger.
runLoggerWith :: LoggerIO -> a
" "runLoggerWith" "0.1.1"))

(assert-equal "-- | @since 0.1.1
runLoggerWith :: LoggerIO -> a
"
              (add-since "runLoggerWith :: LoggerIO -> a
" "runLoggerWith" "0.1.1"))

(assert-equal "-- | @since 0.1.1
instance MonadLogger (Eff es)
"
              (add-since "instance MonadLogger (Eff es)
" "MonadLogger (Eff es)" "0.1.1"))

(let ((source "module M (module Control.Monad.Logger, runLoggerWith) where

runLoggerWith :: LoggerIO -> a
"))
  (assert-false (is-local-declaration source "logInfoN"))
  (assert-true (is-local-declaration source "runLoggerWith")))

(format t "All tests passed.~%")
