#lang racket/gui
(require racket/gui/base)

(define bitmap (read-bitmap "D:/projects/SICP/xiaolajiao.jpg"))
(define DC null)  ;;全局变量

;;创建窗口
(define f (new frame%
               [label "一个图形语言"]
               [width 660]
               [height 660]
               ))
;(new message% [parent f] [label bitmap])
;创建canvas画布
(new canvas% [parent f]
     [paint-callback
      (lambda (canvas dc)
        (set! DC dc)
        ;(send dc draw-bitmap bitmap 0 0)
        ;;(send dc draw-line 0 0 700 700)
        ;(draw-line 0 0 700 700)
        ((square-limit wave 6) frame)
        )])
;;显示
(send f show #t)

;; 基本过程draw-line
(define (draw-line x1 y1 x2 y2)
  (send DC draw-line x1 y1 x2 y2))

;;向量
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons (* s (xcor-vect v))
        (* s (ycor-vect v))))

;; test code for vect
;(define v1 (make-vect 0 1))
;(define v2 (make-vect 3 4))
;(add-vect v1 v2)
;(scale-vect 3 v1)

;;线段
(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

;;框架
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))
  
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
;; test code for frame
(define frame (make-frame (make-vect 60 60) (make-vect 500 0) (make-vect 0 500)))
;(origin-frame frame)
;(edge1-frame frame)
;(edge2-frame frame)
;((frame-coord-map frame) (make-vect 0.5 0.5))

;; 画家wave
(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        (xcor-vect ((frame-coord-map frame) (start-segment segment)))
        (ycor-vect ((frame-coord-map frame) (start-segment segment)))
        (xcor-vect ((frame-coord-map frame) (end-segment segment)))
        (ycor-vect ((frame-coord-map frame) (end-segment segment)))
        ))
     segment-list)))

(define wave
  (let ((segment-list
         (list (make-segment (make-vect 0 0.2) (make-vect 0.2 0.1))
               (make-segment (make-vect 0.2 0.1) (make-vect 0.4 0.4))
               (make-segment (make-vect 0.4 0.4) (make-vect 0.4 0))
               (make-segment (make-vect 0.6 0) (make-vect 0.6 0.4))
               (make-segment (make-vect 0.6 0.4) (make-vect 0.9 0.5))
               (make-segment (make-vect 0.9 0.5) (make-vect 1 0.4))
               (make-segment (make-vect 0 0.6) (make-vect 0.2 0.2))
               (make-segment (make-vect 0.2 0.2) (make-vect 0.4 0.6))
               (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
               (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1))
               (make-segment (make-vect 0.6 1) (make-vect 0.7 0.8))
               (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
               (make-segment (make-vect 0.6 0.6) (make-vect 0.9 0.7))
               (make-segment (make-vect 0.9 0.7) (make-vect 1 0.8))
               )))
    (segment->painter segment-list)))


 ;;框架变换
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
;; 反转画家
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)   ;;new-origin
                     (make-vect 1 1)   ;;new end of edge1
                     (make-vect 0 0))) ;;new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ;new origin
                     (make-vect 0.0 0.0)   ;new end of edge1
                     (make-vect 1.0 1.0))) ;new end of edge2

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0 0)
                              split-point
                              (make-vect 0 1)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0)
                              (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((paint-bottom
         (transform-painter painter1
                            (make-vect 0 0)
                            (make-vect 1 0)
                            (make-vect 0 0.5)))
        (paint-top
         (transform-painter painter2
                            (make-vect 0 0.5)
                            (make-vect 1 0.5)
                            (make-vect 0 1))))        
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (flip-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flip-pairs wave))
