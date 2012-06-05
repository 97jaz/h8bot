#lang racket/base

(require racket/list
         racket/set
         racket/match)

(provide (all-defined-out))

(define flip-table
  (hash
   #\u0021  #\u00A1
   #\u0022  #\u201E
   #\u0026  #\u214B
   #\u0027  #\u002C
   #\u0028  #\u0029
   #\u002E  #\u02D9
   #\u0033  #\u0190
   #\u0034  #\u152D
   #\u0036  #\u0039
   #\u0037  #\u2C62
   #\u003B  #\u061B
   #\u003C  #\u003E
   #\u003F  #\u00BF
   #\u0041  #\u2200
   #\u0042  #\U10412
   #\u0043  #\u2183
   #\u0044  #\u25D6
   #\u0045  #\u018E
   #\u0046  #\u2132
   #\u0047  #\u2141
   #\u004A  #\u017F
   #\u004B  #\u22CA
   #\u004C  #\u2142
   #\u004D  #\u0057
   #\u004E  #\u1D0E
   #\u0050  #\u0500
   #\u0051  #\u038C
   #\u0052  #\u1D1A
   #\u0054  #\u22A5
   #\u0055  #\u2229
   #\u0056  #\u1D27
   #\u0059  #\u2144
   #\u005B  #\u005D
   #\u005F  #\u203E
   #\u0061  #\u0250
   #\u0062  #\u0071
   #\u0063  #\u0254
   #\u0064  #\u0070
   #\u0065  #\u01DD
   #\u0066  #\u025F
   #\u0067  #\u0183
   #\u0068  #\u0265
   #\u0069  #\u0131
   #\u006A  #\u027E
   #\u006B  #\u029E
   #\u006C  #\u0283
   #\u006D  #\u026F
   #\u006E  #\u0075
   #\u0072  #\u0279
   #\u0074  #\u0287
   #\u0076  #\u028C
   #\u0077  #\u028D
   #\u0079  #\u028E
   #\u007B  #\u007D
   #\u203F  #\u2040
   #\u2045  #\u2046
   #\u2234  #\u2235))

(define (weird-map)
  (define normal " abcdefghijklmnopqrstuvwxyz!?*<>.,=+-()")
  (define options
    (vector 
     " αв¢∂єƒgнιנкℓмησρqяѕтυνωχуz!؟●«».,=+-()"
     " 48(d3f9h!jk1mn0pqr57uvwxy2!?*<>.,=+-()"
     " ÁßČĎĔŦĞĤĨĴĶĹМŃŐРQŔŚŤÚVŴЖŶŹ!؟●«».,=+-()"
     " ค๒ς๔єŦﻮђเןкl๓ภ๏קợгรtยשฬץאz!؟●«».,=+-()"
     " äbċdëfġhïjklmnöpqrstüvwxÿż!?*<>.,=+-()"
     " ábćdéfghíjklmńőpqŕśtúvwxýź!?*<>.,=+-()"
     " ΛBᄃDΣFGΉIJΚᄂMПӨPQЯƧƬЦVЩXΥZ!?*◁▷.,=+-()"
     " ﾑ乃cd乇ｷgんﾉﾌズﾚﾶ刀oｱq尺丂ｲu√wﾒﾘ乙!?*<>.,=+-()"
     " ⓐⓑ©ⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ!?⊛<>๏,⊜⊕⊝()"))
  (define alpha (vector-ref options (random (vector-length options))))
  (define tbl
    (for/hasheqv ([x (in-list (string->list normal))]
                  [y (in-list (string->list alpha))])
      (values x y)))
  (λ (c) (hash-ref tbl c c)))

(define (metal txt)
  (let* ([cs (for/fold ([cs '()]) ([c (in-string txt)])
               (cond [(or (and (memv c '(#\a #\e #\i #\y #\A #\E #\I #\Y)) (< (random) 0.2))
                          (and (memv c '(#\u #\U))                         (< (random) 0.4))
                          (and (memv c '(#\o #\O))                         (< (random) 0.6)))
                      (list* #\u0308 c cs)]
                     [(memv c '(#\o #\O))
                      (list* #\♁ cs)]
                     [(and (memv c '(#\s #\S)) (< (random) 0.7))
                      (list* #\⚡ cs)]
                     [(and (memv c '(#\t #\T)) (< (random) 0.7))
                      (list* #\☩ cs)]
                     [(and (memv c '(#\x #\X)) (< (random) 0.7))
                      (list* #\⚔ cs)]
                     [else
                      (list* c cs)]))]
         [s (string-normalize-nfkc (list->string (reverse cs)))]
         [s (if (< (random) 0.3)
                (string-upcase s)
                s)]
         [s (cond [(< (random) 0.1) (string-append "☠ " s " ☠")]
                  [(< (random) 0.1) (string-append "☢ " s " ☢")]
                  [(< (random) 0.1) (string-append "☣ " s " ☣")]
                  [else s])])
    s))

  
  
              
          
            
        
  