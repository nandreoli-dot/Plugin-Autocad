;;; ============================================================================
;;;  REDET.LSP  -  Detalles ampliados a partir de un contorno circular
;;;  AutoCAD (AutoLISP / Visual LISP - ActiveX Integrado)        Version 3.3
;;; ============================================================================

(vl-load-com)

;;; ------------------------------------------------------------- Ajustes ------
(setq *redet-capa*  "ARQ-DETALLES")   ; Capa de llamadas y detalles
(setq *redet-color* 7)                ; Color si la capa se crea (7 = White / Blanco)
(if (null *redet-factor*)   (setq *redet-factor* 2.0))    ; Ultimo factor usado
(if (null *redet-esc-orig*) (setq *redet-esc-orig* 50.0)) ; Ultima escala 1:N

;;; ---------------------------------------------------------- Utilidades ------

(defun redet:setvar (var val)
  (vl-catch-all-apply 'setvar (list var val))
)

(defun redet:fin-deshacer (doc)
  (while (= 8 (logand 8 (getvar "UNDOCTL")))
    (vla-EndUndoMark doc)
  )
)

(defun redet:num->str (x / s)
  (setq s (rtos x 2 4))
  (if (vl-string-search "." s)
    (setq s (vl-string-right-trim "." (vl-string-right-trim "0" s)))
  )
  (cond
    ((= (substr s 1 1) ".")  (setq s (strcat "0" s)))
    ((= (substr s 1 2) "-.") (setq s (strcat "-0" (substr s 2))))
  )
  s
)

(defun redet:redondear (x / p paso)
  (setq p (fix (/ (log x) (log 10.0))))
  (if (< x 1.0) (setq p (1- p)))
  (setq paso (expt 10.0 (1- p)))
  (* paso (fix (+ 0.5 (/ x paso))))
)

(defun redet:limpiar-nombre (s)
  (vl-string-trim " " (vl-string-translate "<>/\\\":;?*|,=`" "_____________" s))
)

(defun redet:nombre-bloque    (nom) (strcat "DETALLE-" nom))
(defun redet:nombre-contenido (nom) (strcat "DETALLE-" nom "-CONTENIDO"))

(defun redet:siguiente-nombre ( / i)
  (setq i 65)
  (while (and (<= i 90) (tblsearch "BLOCK" (redet:nombre-bloque (chr i))))
    (setq i (1+ i))
  )
  (if (<= i 90)
    (chr i)
    (progn
      (setq i 1)
      (while (tblsearch "BLOCK" (redet:nombre-bloque (itoa i)))
        (setq i (1+ i))
      )
      (itoa i)
    )
  )
)

(defun redet:preparar-capa (doc nom / capa)
  (if (tblsearch "LAYER" nom)
    (setq capa (vla-Item (vla-get-Layers doc) nom))
    (progn
      (setq capa (vla-Add (vla-get-Layers doc) nom))
      (vla-put-Color capa *redet-color*)
    )
  )
  (if (eq (vla-get-Lock capa) :vlax-true)     (vla-put-Lock capa :vlax-false))
  (if (eq (vla-get-LayerOn capa) :vlax-false) (vla-put-LayerOn capa :vlax-true))
  (if (eq (vla-get-Freeze capa) :vlax-true)   (vla-put-Freeze capa :vlax-false))
  capa
)

(defun redet:contorno-valido-p (ent / ed tip nor)
  (setq ed  (entget ent)
        tip (cdr (assoc 0 ed))
        nor (cdr (assoc 210 ed)))
  (and
    (member tip '("LWPOLYLINE" "CIRCLE"))
    (or (null nor) (equal (abs (caddr nor)) 1.0 1e-8))
    (or (= tip "CIRCLE")
        (= 1 (logand 1 (cdr (assoc 70 ed))))
        (equal (vlax-curve-getStartPoint ent) (vlax-curve-getEndPoint ent) 1e-6)
    )
  )
)

(defun redet:depurar (pts / out)
  (foreach p pts
    (if (or (null out) (> (distance p (car out)) 1e-6))
      (setq out (cons p out))
    )
  )
  (if (and (cdr out) (<= (distance (car out) (last out)) 1e-6))
    (setq out (cdr out))
  )
  (reverse out)
)

(defun redet:puntos-contorno (ent / ed obj nv fin i b k j pts)
  (setq ed (entget ent))
  (if (= (cdr (assoc 0 ed)) "CIRCLE")
    (progn
      (setq i 0)
      (repeat 144
        (setq pts (cons (vlax-curve-getPointAtParam ent (* 2.0 pi (/ i 144.0))) pts)
              i   (1+ i))
      )
    )
    (progn
      (setq obj (vlax-ename->vla-object ent)
            nv  (cdr (assoc 90 ed))
            fin (vlax-curve-getEndParam ent)
            i   0)
      (repeat nv
        (setq pts (cons (vlax-curve-getPointAtParam ent (float i)) pts)
              b   (vla-GetBulge obj i))
        (if (and (/= b 0.0) (<= (1+ i) fin))
          (progn
            (setq k (max 2 (fix (+ 0.999 (/ (* 4.0 (atan (abs b))) (/ pi 36.0)))))
                  j 1)
            (repeat (1- k)
              (setq pts (cons (vlax-curve-getPointAtParam ent (+ i (/ (float j) k))) pts)
                    j   (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (redet:depurar
    (mapcar '(lambda (p) (list (car p) (cadr p))) (vl-remove nil (reverse pts)))
  )
)

(defun redet:transformar (p base dest f)
  (list (+ (car dest)  (* f (- (car p)  (car base))))
        (+ (cadr dest) (* f (- (cadr p) (cadr base))))
        0.0)
)

(defun redet:arreglo (tipo lst)
  (vlax-make-variant
    (vlax-safearray-fill (vlax-make-safearray tipo (cons 0 (1- (length lst)))) lst)
  )
)

(defun redet:ss->objetos (ss / i lst)
  (setq i (sslength ss))
  (while (> i 0)
    (setq i   (1- i)
          lst (cons (vlax-ename->vla-object (ssname ss i)) lst))
  )
  lst
)

(defun redet:dibujar-contorno ( / ult ent)
  (setq ult (entlast))
  (redet:setvar "CMDECHO" 1)
  (princ "\nDibuje el contorno del detalle. Haga clic para el centro y luego defina el radio.")
  (command "_.CIRCLE")
  (while (> (getvar "CMDACTIVE") 0) (command pause))
  (redet:setvar "CMDECHO" 0)
  (setq ent (entlast))
  (if (and ent
           (not (equal ent ult))
           (= "CIRCLE" (cdr (assoc 0 (entget ent)))))
    (progn
      (vla-put-Layer (vlax-ename->vla-object ent) *redet-capa*)
      (vla-put-Color (vlax-ename->vla-object ent) 7)
      ent
    )
    (progn (princ "\nNo se dibujo ningun contorno circular.") nil)
  )
)

(defun redet:pedir-contorno ( / sel ent res fin)
  (while (not fin)
    (setvar "ERRNO" 0)
    (initget "Dibujar")
    (setq sel (entsel "\nSeleccione el contorno (circulo o polilinea cerrada) o [Dibujar] <Dibujar>: "))
    (cond
      ((and (null sel) (= 7 (getvar "ERRNO")))
       (princ "\nNo se selecciono ningun objeto.")
      )
      ((or (null sel) (= sel "Dibujar"))
       (if (setq ent (redet:dibujar-contorno)) (setq res (list ent T)))
       (setq fin T)
      )
      ((redet:contorno-valido-p (car sel))
       (setq res (list (car sel) nil)
             fin T)
      )
      (T (princ "\nDebe ser un circulo o una polilinea cerrada, en el plano XY."))
    )
  )
  res
)

(defun redet:seleccionar (ent pts / ss)
  (command "_.ZOOM" "_O" ent "")
  (command "_.ZOOM" "0.8x")
  (setq ss (ssget "_CP" pts '((-4 . "<NOT") (0 . "VIEWPORT") (-4 . "NOT>"))))
  (command "_.ZOOM" "_P")
  (command "_.ZOOM" "_P")
  (if (and ss (ssmemb ent ss)) (ssdel ent ss))
  (if (and ss (> (sslength ss) 0)) ss)
)

(defun redet:pedir-nombre ( / def nom)
  (setq def (redet:siguiente-nombre))
  (while (not nom)
    (setq nom (strcase (redet:limpiar-nombre
                (getstring T (strcat "\nIdentificacion del detalle (letra o numero) <" def ">: ")))))
    (if (= nom "") (setq nom def))
    (if (or (tblsearch "BLOCK" (redet:nombre-bloque nom))
            (tblsearch "BLOCK" (redet:nombre-contenido nom)))
      (progn
        (princ (strcat "\nYa existe el detalle " nom ". Indique otro nombre."))
        (setq nom nil)
      )
    )
  )
  nom
)

(defun redet:pedir-escala ( / r orig det f)
  (initget 6 "Escalas")
  (setq r (getreal (strcat "\nFactor de ampliacion o [Escalas] <"
                           (redet:num->str *redet-factor*) ">: ")))
  (if (= r "Escalas")
    (progn
      (initget 6)
      (setq orig (getreal (strcat "\nEscala del dibujo original, 1:N. Indique N <"
                                  (redet:num->str *redet-esc-orig*) ">: ")))
      (if (not orig) (setq orig *redet-esc-orig*))
      (setq det (/ orig *redet-factor*))
      (initget 6)
      (setq r (getreal (strcat "\nEscala del detalle, 1:N. Indique N <"
                               (redet:num->str det) ">: ")))
      (if r (setq det r))
      (setq f              (/ orig det)
            *redet-esc-orig* orig
            *redet-factor*   f)
      (list f (strcat "1:" (redet:num->str det)))
    )
    (progn
      (setq f              (if r r *redet-factor*)
            *redet-factor* f)
      (list f (if (>= f 1.0)
                (strcat (redet:num->str f) ":1")
                (strcat "1:" (redet:num->str (/ 1.0 f)))))
    )
  )
)

(defun redet:crear-marco (spc ent base dest f / ed obj nv nor sig i q lst pl)
  (setq ed (entget ent))
  (if (= (cdr (assoc 0 ed)) "CIRCLE")
    (vla-AddCircle spc
      (vlax-3d-point (redet:transformar (trans (cdr (assoc 10 ed)) ent 0) base dest f))
      (* f (cdr (assoc 40 ed)))
    )
    (progn
      (setq obj (vlax-ename->vla-object ent)
            nv  (cdr (assoc 90 ed))
            nor (cdr (assoc 210 ed))
            sig (if (and nor (< (caddr nor) 0.0)) -1.0 1.0)
            i   0)
      (repeat nv
        (setq q   (redet:transformar (vlax-curve-getPointAtParam ent (float i)) base dest f)
              lst (append lst (list (car q) (cadr q)))
              i   (1+ i))
      )
      (setq pl (vla-AddLightWeightPolyline spc (redet:arreglo vlax-vbDouble lst))
            i  0)
      (repeat nv
        (vla-SetBulge pl i (* sig (vla-GetBulge obj i)))
        (setq i (1+ i))
      )
      (vla-put-Closed pl :vlax-true)
      pl
    )
  )
)

;;; ------------------------------------------------------------- Comando ------

(defun c:REDET ( / *error* salir restaurar doc spc vars vals ucs-cambiado etapa
                   sel bnd nuevo pts xs ys xmin xmax ymin ymax base ss
                   nom esc f def titulo alto dest
                   nomBlq nomCont blqCont r insCont marco ptTxt txt insDet 
                   ptsFlat polyMask blqFinal arrObj q)

  (defun restaurar ()
    (if ucs-cambiado
      (progn (command-s "_.UCS" "_P") (setq ucs-cambiado nil))
    )
    (if vals (mapcar 'redet:setvar vars vals))
    (if doc (redet:fin-deshacer doc))
  )

  (defun *error* (msg)
    (if (and bnd nuevo (not etapa) (entget bnd)) (entdel bnd))
    (restaurar)
    (cond
      ((wcmatch (strcase msg) "*QUIT*,*EXIT*"))
      ((wcmatch (strcase msg) "*BREAK*,*CANCEL*") (princ "\nComando cancelado."))
      (T (princ (strcat "\nError: " msg)))
    )
    (if etapa (princ "\nEl detalle quedo incompleto: use U para deshacerlo."))
    (princ)
  )

  (defun salir (msg) (princ msg) (exit))

  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        vars '("CMDECHO" "OSMODE" "UCSFOLLOW" "PLINETYPE")
        vals (mapcar 'getvar vars))
  (redet:fin-deshacer doc)
  (vla-StartUndoMark doc)
  (redet:setvar "CMDECHO" 0)

  (if (= 0 (getvar "WORLDUCS"))
    (progn
      (redet:setvar "UCSFOLLOW" 0)
      (command "_.UCS" "_W")
      (setq ucs-cambiado T)
    )
  )
  (setq spc (if (= (vla-get-ActiveSpace doc) acModelSpace)
              (vla-get-ModelSpace doc)
              (vla-get-PaperSpace doc)))
  (redet:preparar-capa doc *redet-capa*)

  (princ "\n--- REDET: Detalle ampliado en bloque ---")

  (if (not (setq sel (redet:pedir-contorno)))
    (salir "\nNo se definio el contorno.")
  )
  (setq bnd   (car sel)
        nuevo (cadr sel)
        pts   (redet:puntos-contorno bnd))
  (if (< (length pts) 3)
    (salir "\nEl contorno necesita al menos 3 vertices.")
  )
  (if (not (setq ss (redet:seleccionar bnd pts)))
    (salir "\nNo hay objetos dentro del contorno.")
  )
  (princ (strcat "\n" (itoa (sslength ss)) " objeto(s) dentro del contorno."))

  (setq xs   (mapcar 'car pts)
        ys   (mapcar 'cadr pts)
        xmin (apply 'min xs)
        xmax (apply 'max xs)
        ymin (apply 'min ys)
        ymax (apply 'max ys)
        base (list (/ (+ xmin xmax) 2.0) (/ (+ ymin ymax) 2.0) 0.0))

  (setq nom    (redet:pedir-nombre)
        esc    (redet:pedir-escala)
        f      (car esc)
        def    (strcat "DETALLE " nom " - ESCALA " (cadr esc))
        titulo (getstring T (strcat "\nTexto del titulo <" def ">: ")))
  (if (= (vl-string-trim " " titulo) "") (setq titulo def))

  (setq def (if *redet-alto*
              *redet-alto*
              (redet:redondear (* 0.05 f (max (- xmax xmin) (- ymax ymin))))))
  (initget 6)
  (setq alto (getdist (strcat "\nAltura del texto del titulo <" (redet:num->str def) ">: ")))
  (if (not alto) (setq alto def))
  (setq *redet-alto* alto)

  (if (not (setq dest (getpoint base "\nPunto donde ubicar el centro del detalle: ")))
    (salir "\nNo se indico la ubicacion.")
  )
  (setq dest (list (car dest) (cadr dest) 0.0))

  (redet:setvar "OSMODE" 0)
  (setq nomBlq  (redet:nombre-bloque nom)
        nomCont (redet:nombre-contenido nom))

  (setq blqCont (vla-Add (vla-get-Blocks doc) (vlax-3d-point base) nomCont)
        r       (vl-catch-all-apply 'vla-CopyObjects
                  (list doc (redet:arreglo vlax-vbObject (redet:ss->objetos ss)) blqCont)))
  (if (vl-catch-all-error-p r)
    (progn
      (vl-catch-all-apply 'vla-Delete (list blqCont))
      (salir (strcat "\nNo se pudieron copiar los objetos: " (vl-catch-all-error-message r)))
    )
  )
  (setq etapa T)

  (setq insCont (vla-InsertBlock spc (vlax-3d-point dest) nomCont f f f 0.0))
  (vla-put-Layer insCont *redet-capa*)

  ;; Aplicar Recorte XCLIP
  (setq ptsFlat nil)
  (foreach p pts
    (setq q (redet:transformar p base dest f))
    (setq ptsFlat (append ptsFlat (list (car q) (cadr q))))
  )
  (setq polyMask (vla-AddLightWeightPolyline spc (redet:arreglo vlax-vbDouble ptsFlat)))
  (vla-put-Closed polyMask :vlax-true)
  (command "_.XCLIP" (vlax-vla-object->ename insCont) "" "_N" "_S" (vlax-vla-object->ename polyMask))
  (vla-Delete polyMask) 

  ;; Marco circular
  (setq marco (redet:crear-marco spc bnd base dest f))
  (vla-put-Layer marco *redet-capa*)
  (vla-put-Color marco 7)

  ;; Texto MTEXT
  (setq ptTxt (list (car dest) (- (cadr dest) (* f (- (cadr base) ymin)) (* 0.8 alto)) 0.0))
  (setq txt (vla-AddMText spc (vlax-3d-point ptTxt) 0.0 titulo))
  (vla-put-Height txt alto)
  (vla-put-AttachmentPoint txt 2) 
  (vla-put-InsertionPoint txt (vlax-3d-point ptTxt))
  (vla-put-Layer txt *redet-capa*)
  (vla-put-Color txt 7)

  ;; Empaquetado final en bloque unico DETALLE-<nombre>
  (setq blqFinal (vla-Add (vla-get-Blocks doc) (vlax-3d-point dest) nomBlq))
  (setq arrObj (redet:arreglo vlax-vbObject (list insCont marco txt)))
  (vl-catch-all-apply 'vla-CopyObjects (list doc arrObj blqFinal))
  
  ;; Limpieza de temporales del modelo
  (vla-Delete insCont)
  (vla-Delete marco)
  (vla-Delete txt)
  
  (setq insDet (vla-InsertBlock spc (vlax-3d-point dest) nomBlq 1.0 1.0 1.0 0.0))
  (vla-put-Layer insDet *redet-capa*)

  (restaurar)
  (princ (strcat "\nListo: detalle " nom " creado exitosamente como bloque " nomBlq))
  (princ)
)

(princ "\nREDET v3.3 LISP cargado correctamente. Escriba REDET para iniciar.")
(princ)