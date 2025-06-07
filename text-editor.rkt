;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname text-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

;; vi-style single line text editor

;; =================
;; Constants:

(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define CURSOR-CHAR "|")

(define WIDTH 400)
(define HEIGHT FONT-SIZE)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

;; EditorMode is one of:
;;  - "normal"
;;  - "insert"
;; interp. the editor current mode

;; <examples are redundant for enumerations>

#;
(define (fn-for-editor-mode em)
  (cond [(string=? em "normal") (...)]
        [(string=? em "insert") (...)]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: "normal"
;;  - atomic distinct: "insert"

(define-struct editor (line cursor mode))
;; Editor is (make-editor String Natural EditorMode)
;; interp. (make-editor line cursor mode) represents the current state of the editor:
;;         line is text string
;;         cursor is index in string
;;         mode is either "normal" or "insert"

(define E0 (make-editor "" 0 "normal"))
(define E1 (make-editor "cat" 1 "normal"))
(define E2 (make-editor "cat" 2 "insert"))

#;
(define (fn-for-editor e)
  (... (editor-line e)   ;String
       (editor-cursor e) ;Natural
       (editor-mode e))) ;EditorMode

;; Template rules used:
;;  - compound: 3 fields

;; =================
;; Functions:

;; Editor -> Editor
;; start the world with (main (make-editor "" 0 "normal"))
;; 
(define (main e)
  (big-bang e              ; Editor
    (to-draw render)       ; Editor -> Image
    (on-key  handle-key))) ; Editor KeyEvent -> Editor

;; Editor -> Image
;; render the editor as an image with the cursor

(check-expect (render E1)
              (overlay/align "left" "center"
                             (text "c|at" FONT-SIZE FONT-COLOR)
                             MTS))

;(define (render e) MTS) ;stub

;; <use template from Editor>

(define (render e)
  (overlay/align "left" "center"
                 (text (string-append (substring (editor-line e) 0 (editor-cursor e))
                                      CURSOR-CHAR
                                      (substring (editor-line e) (editor-cursor e)))
                       FONT-SIZE FONT-COLOR)
                 MTS))

;; Editor KeyEvent -> Editor
;; handle keys in normal mode
;; In normal mode: "h" and "l" move cursor, "i" enters insert mode
;; In insert mode: any chars inserts at cursor, "escape" returns to normal
(check-expect (handle-key E1 "i") (make-editor "cat" 1 "insert"))
(check-expect (handle-key E2 "\b")
              (make-editor "ct" 1 "insert"))
(check-expect (handle-key (make-editor "cat" 1 "insert") "escape")
              (make-editor "cat" 1 "normal"))
(check-expect (handle-key E1 "x")
              (make-editor "ct" 1 "normal"))

;(define (handle-key e ke) e) ;stub

#;
(define (handle-key e ke)
  (cond [(key=? ke "i") (... e)]
        [(key=? ke "h") (... e)]
        [(key=? ke "l") (... e)]
        [(key=? ke "escape") (... e)]
        [else (... ws)]))

(define (handle-key e ke)
  (if (string=? (editor-mode e) "normal") ;; normal mode
      (cond [(key=? ke "i")
             (make-editor (editor-line e) (editor-cursor e) "insert")]
            [(key=? ke "h")
             (make-editor (editor-line e)
                          (max (- (editor-cursor e) 1) 0)
                          "normal")]
            [(key=? ke "l")
             (make-editor (editor-line e)
                          (min (+ (editor-cursor e) 1) (string-length (editor-line e)))
                          "normal")]
            [(key=? ke "x")
             (if (< (editor-cursor e) (string-length (editor-line e)))
                 (make-editor (string-append (substring (editor-line e) 0 (editor-cursor e))
                                             (substring (editor-line e) (+ (editor-cursor e) 1)))
                              (editor-cursor e)
                              "normal")
                 e)]
            [else e])
      (cond [(key=? ke "escape") ;; insert mode
             (make-editor (editor-line e) (editor-cursor e) "normal")]
            [(key=? ke "\b")
             (if (> (editor-cursor e) 0)
                 (make-editor (string-append (substring (editor-line e) 0 (- (editor-cursor e) 1))
                                             (substring (editor-line e) (editor-cursor e)))
                              (- (editor-cursor e) 1)
                              "insert")
                 e)]
            [(= (string-length ke) 1)
             (make-editor (string-append (substring (editor-line e) 0 (editor-cursor e))
                                         ke
                                         (substring (editor-line e) (editor-cursor e)))
                          (+ (editor-cursor e) 1)
                          "insert")]
            [else e])))
