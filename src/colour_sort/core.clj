(ns colour-sort.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (let [subject (q/load-image "vangogh.jpg")
        pix (q/pixels subject)
        shuf (shuffle (vec pix))
        ]
    (dotimes [i (count pix)]
      (aset pix i (nth shuf i)))

    (q/update-pixels subject)
    {:img subject
     }))

(defn colour-dist [a b]
  (Math/sqrt 
    (+
     (Math/pow (- (q/red a) (q/red b)) 2)
     (Math/pow (- (q/green a) (q/green b)) 2)
     (Math/pow (- (q/blue a) (q/blue b)) 2))))

(defn adj-col [w h x y]
  (filterv (complement nil?)
          (for [x' [-2 -1 0 1 2] y' [-2 -1 0 1 2]]
            (if (= [0 0] [x' y'])
              nil
              (vector (mod (- x x') w) (mod (+ y y') h)))) ))


(defn pos-score [img colour x y]
  (let [colour-dist (partial colour-dist colour)
        adj-col (partial adj-col (.-width img) (.-height img))]
    (reduce +
      (map #(let [[x y] %] (colour-dist (q/get-pixel img x y)))
            (adj-col x y)))))



(defn update-state [state]
  (let [img (:img state)
        pix (q/pixels img)
        pos-score (partial pos-score img)]

    (dotimes [i 1000]
      (let [x1 (rand-int (.-width img))
            y1 (rand-int (.-height img))
            col1 (q/get-pixel img x1 y1)
            ; x2 (rand-int (.-width img))
            x2 (int (mod (+ x1 (q/random -10 10)) (.-width img)))
            ; x2 (int (mod (+ (q/random 0 0) (q/map-range (q/cos (+ (q/random 2) (/ (q/brightness col1) 13))) -1 1 0 (.-width img))) (.-width img)))
            ; y2 (rand-int (.-height img))
            y2 (int (mod (+ y1 (q/random -10 10)) (.-height img)))
            ; y2 (int (mod (+ (q/random 0 0) (q/map-range (q/sin (+ (q/random 2) (/ (q/red col1) 9))) -1 1 0 (.-height img))) (.-height img)))
            col2 (q/get-pixel img x2 y2)
            ]
        ; if swapping is less jumbled than not swapping
        (when (< (+ (pos-score col1 x2 y2)
                    (pos-score col2 x1 y1))
                 (+ (pos-score col2 x2 y2)
                    (pos-score col1 x1 y1)))
          (q/set-pixel 
            img x2 y2 col1)
          (q/set-pixel 
            img x1 y1 col2)
          )
        )
      )

    ; (q/background 0)

    ; (aset pix a bcol)
    ; (aset pix b acol)
    ; (aset pix (rand-int (alength pix)) 0)


    ; (q/set-pixel (:img state) (rand 30) (rand 30) 
    ;              (q/color 255 0 0))
    (q/update-pixels img)
    state
    ))


(defn draw-state [state]
  (q/background 2)
  (q/image (:img state) 0 0)
  ; (q/text "test" 20 20)
  ; (q/ellipse 20 20 20 20)
  ;; (print state)
  (q/stroke 255)
  (q/fill 255)
  ; (q/save "55000.png")
  (q/text (str (q/frame-count)) 20 20)
  )

(q/defsketch colour-sort
  :title "Test"
  :size [402 402]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]
  )
