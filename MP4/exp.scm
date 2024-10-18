(define stringDeg->offset
    (lambda (stringDeg)
        (apply + (map char->integer (string->list stringDeg)))
    )
)
(stringDeg->offset "i")
;305 VIII 433 viii
;232 VII  328 vii
;159 VI   223 vi
;86  V    118 v
;159 IV   223 iv
;219 III  315 iii
;146 II   210 ii
;73  I    105 i