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
             (math vector)
             (hoot ffi)
             (hoot hashtables)
             (ice-9 match))

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

(define *clicks* 0)
(define *template* '())

;; Click
(define (template-click)
  `(div
    (p ,(number->string *clicks*) " clicks")
    (button (@ (click ,(lambda (event)
                            (set! *clicks* (+ *clicks* 1))
                            (render))))
                "Click me!")))

;; Task
(define (render)
  (let ((old (get-element-by-id "container")))
    (unless (external-null? old) (remove! old))
  (append-child! (document-body) (sxml->dom (wrap-template *template*)))))

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
               ;; Strikethrough if task is done.
               ,(if (task-done? task)
                    `(s ,(task-name task))
                    (task-name task)))
         (a (@ (href "#")
               ;; Remove task on click.
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

;; Main
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
    (div (@ (id "application")) ,(template))))

(set! *template* template-task)
(render)
