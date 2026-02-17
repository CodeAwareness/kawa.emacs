;;; test-peer-selection.el --- Test peer selection functionality -*- lexical-binding: t -*-

;; This file tests the peer selection functionality in Kawa Code for Emacs

(require 'kawacode)

(defun test-peer-selection-handling ()
  "Test that peer selection messages are handled correctly."
  (let ((test-peer-data '((_id . "67bc3432cf9b5efe459008dd")
                          (name . "Alice")
                          (company . "Kawa Code")
                          (phone . "123-123-1234")
                          (email . "alice@codeawareness.com")
                          (createdAt . "2025-02-24T08:56:18.540Z")
                          (active . t)
                          (updatedAt . "2025-08-29T00:38:56.447Z")
                          (lang . "en"))))
    ;; Test peer selection
    (kawacode--handle-peer-select test-peer-data)
    (assert (equal kawacode--selected-peer test-peer-data)
            "Selected peer should be set correctly")
    
    ;; Test peer unselection
    (kawacode--handle-peer-unselect)
    (assert (null kawacode--selected-peer)
            "Selected peer should be cleared on unselect")
    
    (message "Peer selection tests passed!")))

(defun test-diff-buffer-creation ()
  "Test that diff buffers are created correctly."
  (let ((test-peer-file "/tmp/test-peer-file.txt")
        (test-user-file "/tmp/test-user-file.txt")
        (test-title "Test Diff"))
    ;; Create test files
    (with-temp-file test-peer-file
      (insert "Hello from peer\nThis is line 2\n"))
    (with-temp-file test-user-file
      (insert "Hello from user\nThis is line 2\nAnd this is line 3\n"))
    
    ;; Test diff view creation
    (kawacode--open-diff-view test-peer-file test-user-file test-title)
    
    ;; Check if diff buffer was created
    (let ((diff-buffer (get-buffer "*Kawa Code Diff: Test Diff*")))
      (assert diff-buffer "Diff buffer should be created")
      (assert (buffer-live-p diff-buffer) "Diff buffer should be live"))
    
    ;; Clean up
    (kawacode--close-diff-buffers)
    (delete-file test-peer-file)
    (delete-file test-user-file)
    
    (message "Diff buffer tests passed!")))

(defun test-ipc-message-parsing ()
  "Test that IPC messages are parsed correctly for peer selection."
  (let ((test-message "{\"flow\":\"req\",\"domain\":\"code\",\"action\":\"peer:select\",\"data\":{\"_id\":\"67bc3432cf9b5efe459008dd\",\"name\":\"Alice\",\"company\":\"Kawa Code\",\"phone\":\"123-123-1234\",\"email\":\"alice@codeawareness.com\",\"createdAt\":\"2025-02-24T08:56:18.540Z\",\"active\":true,\"updatedAt\":\"2025-08-29T00:38:56.447Z\",\"lang\":\"en\"},\"err\":\"\",\"caw\":\"0\"}"))
    ;; Mock the peer selection handler to track calls
    (let ((peer-select-called nil))
      (flet ((kawacode--handle-peer-select (data)
               (setq peer-select-called t)
               (assert (equal (alist-get 'name data) "Alice")
                       "Peer name should be parsed correctly")))
        (kawacode--handle-ipc-message test-message)
        (assert peer-select-called "Peer selection handler should be called")))
    
    (message "IPC message parsing tests passed!")))

(defun test-event-handler-registration ()
  "Test that event handlers are registered correctly."
  ;; Initialize event handlers
  (kawacode--init-event-handlers)
  
  ;; Test that handlers are registered
  (assert (gethash "peer:select" kawacode--events-table)
          "peer:select handler should be registered")
  (assert (gethash "branch:select" kawacode--events-table)
          "branch:select handler should be registered")
  (assert (gethash "auth:logout" kawacode--events-table)
          "auth:logout handler should be registered")
  
  (message "Event handler registration tests passed!"))

(defun run-all-peer-selection-tests ()
  "Run all peer selection tests."
  (interactive)
  (message "Running peer selection tests...")
  (test-peer-selection-handling)
  (test-diff-buffer-creation)
  (test-ipc-message-parsing)
  (test-event-handler-registration)
  (message "All peer selection tests passed!"))

(provide 'test-peer-selection)
