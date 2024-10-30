;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Example game showing off several common game programming things.
;;;
;;; Code:

(use-modules (scheme base)
             (dom document)
             (dom element)
             (dom event)
             (dom window)
             (math vector)
             (hoot ffi)
             (hoot debug)
             (hoot hashtables)
             (srfi srfi-9)
             (ice-9 match))


(define *template* '())
(define (wrap-template template)
  `(div (@ (id "container"))
    (button (@ (click ,(lambda (event)
                            (set! *template* template-click)
                            (render))))
                "Click")
    (button (@ (click ,(lambda (event)
                            (set! *template* template-task)
                            (render))))
                "Task")
    (button (@ (click ,(lambda (event)
                            (set! *template* template-ascii-art)
                            (render))))
                "Ascii art")
    (button (@ (click ,(lambda (event)
                            (set! *template* template-farm)
                            (render))))
                "Farm")
    (div (@ (id "application")) ,(template))))

(define (render)
  (let ((old (get-element-by-id "container")))
    (unless (external-null? old) (remove! old))
  (append-child! (document-body) (sxml->dom (wrap-template *template*)))))

(define (sxml->dom exp)
  (match exp
    ;; The simple case: a string representing a text node.
    ((? string? str)
     (make-text-node str))
    ;; An element tree.  The first item is the HTML tag.
    (((? symbol? tag) . body)
     ;; Create a new element with the given tag.
     (let ((elem (make-element (symbol->string tag))))
       (define (add-children children)
         ;; Recursively call sxml->dom for each child node and
         ;; append it to elem.
         (for-each (lambda (child)
                     (append-child! elem (sxml->dom child)))
                   children))
       (match body
         ;; '@' denotes an attribute list.  Child nodes follow.
         ((('@ . attrs) . children)
          ;; Set attributes.
          (for-each (lambda (attr)
                      (match attr
                        ;; Attributes are (symbol string) tuples.
                        (((? symbol? name) (? string? val))
                         (set-attribute! elem
                                         (symbol->string name)
                                         val))
                        (((? symbol? name) (? boolean? val))
                         (set-attribute! elem
                                         (symbol->string name)
                                         val))
                        (((? symbol? name) (? procedure? proc))
                         (add-event-listener! elem
                                       (symbol->string name)
                                       (procedure->external proc)))))
                    attrs)
          (add-children children))
         ;; No attributes, just a list of child nodes.
         (children (add-children children)))
       elem))))

;; Click
(define *clicks* 0)
(define (template-click)
  `(div
    (p ,(number->string *clicks*) " clicks")
    (button (@ (click ,(lambda (event)
                            (set! *clicks* (+ *clicks* 1))
                            (render))))
                "Click me!")))

;; Task
(define-record-type <task>
  (make-task name done?)
  task?
  (name task-name)
  (done? task-done? set-task-done!))

(define *tasks* '())
(define (add-task! task)
  (set! *tasks* (cons task *tasks*)))
(define (remove-task! task)
  (set! *tasks* (delq task *tasks*)))

(define (template-task)
  (define (task-template task)
    `(li (input (@ (type "checkbox")
                   (change ,(lambda (event)
                              (let* ((checkbox (event-target event))
                                     (checked? (element-checked? checkbox)))
                                (set-task-done! task checked?)
                                (render))))
                   (checked ,(task-done? task))))
         (span (@ (style "padding: 0 1em 0 1em;"))
               ,(if (task-done? task)
                    `(s ,(task-name task))
                    (task-name task)))
         (a (@ (href "#")
               (click ,(lambda (event)
                         (remove-task! task)
                         (render))))
            "remove")))
  `(div
    (h2 "Tasks")
    ;; Tasks are stored in reverse order.
    (ul ,@(map task-template (reverse *tasks*)))
    (input (@ (id "new-task")
              (placeholder "Write more task")))
    ;; Add new task on click
    (button (@ (click ,(lambda (event)
                         (let* ((input (get-element-by-id "new-task"))
                                (name (element-value input)))
                           (unless (string=? name "")
                             (add-task! (make-task name #f))
                             (set-element-value! input "")
                             (render))))))
            "Add task")))


;; render ascii art
(define (template-ascii-art)
  (define ink-script '(
                       "(\\                         "
                       "\\'\\                        "
                       " \\'\\     __________        "
                       " / '|   ()_________)       "
                       " \\ '/    \\ ~~~~~~~~ \\      "
                       "   \\       \\ ~~~~~~   \\    "
                       "   ==).      \\__________\\  "
                       "  (__)       ()__________) "
                       ))
  `(pre ,(string-join ink-script "\n")))

;; render farm
(define (template-farm)
  (define (wrap-line line title)
    `(div (@ (style "font-family: monospace;
                     white-space: pre;     
                     line-height: 2;        
                     cursor: default;
                     width: fit-content;")     
             (title ,title))
          ,line))
  
  `(div (@ (style "position: relative;
                   width: fit-content;"))     
        ,(wrap-line "      /\\      " "Farm Warehouse ")
        ,(wrap-line "     /  \\     " "Farm Warehouse ")
        ,(wrap-line "    /____\\    " "Farm Warehouse ")
        ,(wrap-line "    |    |    " "Farm Warehouse ")
        ,(wrap-line "  __|    |__  " "Farm Warehouse ")
        ,(wrap-line " |    __    | " "Farm Warehouse ")
        ,(wrap-line " |[]      []| " "Farm Warehouse ")
        ,(wrap-line " |__________| " "Farm Warehouse ")
     (button (@ (style "position: absolute;
                       top: 10px;         
                       left: 10%;
                       transform: translateX(-50%);
                       width: 30px;
                       height: 40px;
                       padding: 0;
                       font-family: monospace;
                       background: #8b4513;  
                       border: 1px solid #654321;
                       color: #deb887;
                       cursor: pointer;")
                (click ,(lambda (event)
                         (dprint "Door clicked!")
                         )))
             "[]"))) 

;; Main
(set! *template* template-task)

(define *update-list* '())
(define dt (/ 1000.0 60.0))

(define-record-type <timeout-function>
  (make-timeout-function interval countdown func)
  timeout-function?
  (interval timeout-function-interval set-timeout-function-interval!)
  (countdown timeout-function-countdown set-timeout-function-countdown!)
  (func timeout-function-func))

(define (plus-clicks)
  (set! *clicks* (+ *clicks* 1)))

(set! *update-list* (append *update-list* (list (make-timeout-function 60 60 plus-clicks))))

(define (update)
  (for-each (lambda (i)
              (dprint "countdown" (timeout-function-countdown i))
              (if (= (timeout-function-countdown i) 0)
                  (begin
                    ((timeout-function-func i))
                    (set-timeout-function-countdown! i (timeout-function-interval i))
                    (if (equal? *template* template-click)
                        (render)))
                  (set-timeout-function-countdown! i (- (timeout-function-countdown i) 1))))
            *update-list*)
  (timeout update-callback dt))

(define update-callback (procedure->external update))
(render)
(timeout update-callback dt)
