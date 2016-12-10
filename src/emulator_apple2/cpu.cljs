(ns emulator-apple2.cpu
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [emulator-apple2.utils :as u]))

;; CPU Registers

(def FLAG_CARRY 2r00000001)
(def FLAG_ZERO 2r00000010)
(def FLAG_INTERRUPT 2r00000100)
(def FLAG_DECIMAL 2r00001000)
(def FLAG_BREAK 2r00010000)
(def FLAG_OVERFLOW 2r01000000)
(def FLAG_SIGN 2r10000000)
(def FLAG_CONTANT 2r00100000)

(def BASE_STACK 0x100)

(def *mem-read-fn* nil)
(def *mem-write-fn* nil)

(defonce *acc* 0)
(defonce *sp* 0xFD)
(defonce *xreg* 0)
(defonce *yreg* 0)
(defonce *pc* 0)
(defonce *status* FLAG_CONTANT)
(defonce *break?* false)

(defn set-flag! [flag]
  (set! *status* (bit-or *status* flag)))

(defn clear-flag! [flag]
  (set! *status* (bit-and-not *status* flag)))

(defn flag-set? [flag]
  (> (bit-and *status* flag) 0))

(defn calc-zero! [value]
  (if (zero? (bit-and value 0xFF))
    (set-flag! FLAG_ZERO)
    (clear-flag! FLAG_ZERO)))

(defn calc-sign! [value]
  (if (> (bit-and value 2r10000000) 0)
    (set-flag! FLAG_SIGN)
    (clear-flag! FLAG_SIGN)))

(defn calc-carry! [value]
  (if (> (bit-and value 16rFF00) 0)
    (set-flag! FLAG_CARRY)
    (clear-flag! FLAG_CARRY)))


(defn calc-overflow!
  "Set the overflow flag if the most significant bit (sign) in a and b is
  diferent from the bit in res. This means that if both a and b have the
  same sign, the result should also have the same sign"
  [a b res]
  (if (zero? (bit-and (bit-xor res a) (bit-xor res b) 2r10000000))
    (clear-flag! FLAG_OVERFLOW)
    (set-flag! FLAG_OVERFLOW)))

(defn read-inc-pc! []
  (let [pc *pc*]
    (set! *pc* (inc *pc*))
    pc))


;; Memory Access

(defn write-mem! [addr val]
  (*mem-write-fn* addr val))

(defn read-mem [addr]
  (*mem-read-fn* addr))

(defn read-mem-word [addr]
  (u/get-word (read-mem addr) (read-mem (inc addr))))


;; Stack

(defn push-stack! [val]
  (write-mem! (+ BASE_STACK *sp*) val)
  (set! *sp* (bit-and (dec *sp*) 0xFF)))

(defn push-word-stack! [val]
  (push-stack! (bit-and (bit-shift-right val 8) 0xFF))
  (push-stack! (bit-and val 0xFF)))

(defn pull-stack! []
  (set! *sp* (bit-and (inc *sp*) 0xFF))
  (read-mem (+ BASE_STACK *sp*)))

(defn pull-word-stack! []
  (u/get-word (pull-stack!) (pull-stack!)))


;; Aderessiong modes

(defn abs-addr []
  (u/get-word (read-mem (read-inc-pc!)) (read-mem (read-inc-pc!))))

(defn absx-addr []
  (+ (abs-addr) *xreg*))

(defn absy-addr []
  (+ (abs-addr) *yreg*))

(defn zpx-addr []
  (bit-and (+ (read-mem (read-inc-pc!)) *xreg*) 16rFF))

(defn zpy-addr []
  (bit-and (+ (read-mem (read-inc-pc!)) *yreg*) 16rFF))

(defn zp-addr []
  (read-mem (read-inc-pc!)))

(defn xind-addr []
  (read-mem-word (zpx-addr)))

(defn indy-addr []
  (bit-and (+ (read-mem-word (zp-addr)) *yreg*) 16rFFFF))

(defn ind-addr []
  (read-mem-word (abs-addr)))

(defn imm-addr []
  (read-inc-pc!))


(defn rel-addr []
  (let [addr (read-mem (read-inc-pc!))]
    (if (> (bit-and addr 2r10000000) 0) (- addr 256) addr)))

(defn imp-addr []
  nil)

(defn acc-addr []
  nil)


;; Operations

(defn- to-decimal [val]
  (+ (if (> (bit-and 0x0F val) 0x09) 0x06 0)
     (if (> (bit-and 0xF0 val) 0x90) 0x60 0)
     val))

(defn adc-val! [val]
  (let [res (+ *acc* val (if (flag-set? FLAG_CARRY) 1 0))]
    (calc-zero! res)
    (calc-sign! res)
    (calc-overflow! *acc* val res)
    ; NES doesn't use decimal mode (+ 1 cycle)
    (if (flag-set? FLAG_DECIMAL)
      (let [dec (to-decimal res)]
        (if (> dec 0x99)
          (set-flag! FLAG_CARRY)
          (clear-flag! FLAG_CARRY))
        (set! *acc* (bit-and dec 0xFF)))
      (do
        (calc-carry! res)
        (set! *acc* (bit-and res 0xFF))))))

(defn adc-op! [addr]
  (adc-val! (read-mem addr)))

(defn and-op! [addr]
  (let [val (bit-and *acc* (read-mem addr))]
    (calc-sign! val)
    (calc-zero! val)
    (set! *acc* val)))

(defn- asl-val! [val]
  (let [res (bit-shift-left val 1)]
    (calc-sign! res)
    (calc-zero! res)
    (calc-carry! res)
    (bit-and res 0xFF)))

(defn asl-op! [addr]
  (write-mem! addr (asl-val! (read-mem addr))))

(defn asl-acc-op! [addr]
  (set! *acc* (asl-val! *acc*)))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bcc-op! [addr]
  (when-not (flag-set? FLAG_CARRY)
    (set! *pc* (+ *pc* addr))))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bcs-op! [addr]
  (when (flag-set? FLAG_CARRY)
    (set! *pc* (+ *pc* addr))))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn beq-op! [addr]
  (when (flag-set? FLAG_ZERO)
    (set! *pc* (+ *pc* addr))))

(defn bit-op! [addr]
  (let [val (read-mem addr)
        res (bit-and *acc* val)
        mask (bit-or FLAG_OVERFLOW FLAG_SIGN)]
    (calc-zero! res)
    (set! *status* (bit-or (bit-and-not *status* mask) (bit-and val mask)))))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bmi-op! [addr]
  (when (flag-set? FLAG_SIGN)
    (set! *pc* (+ *pc* addr))))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bne-op! [addr]
  (when-not (flag-set? FLAG_ZERO)
    (set! *pc* (+ *pc* addr))))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bpl-op! [addr]
  (when-not (flag-set? FLAG_SIGN)
    (set! *pc* (+ *pc* addr))))

(defn brk-op! [addr]
  (push-word-stack! (bit-and (inc *pc*) 0xFFFF))
  (push-stack! (bit-or *status* FLAG_BREAK))
  (set-flag! FLAG_INTERRUPT)
  (set! *pc* (read-mem-word 0xFFFE)))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bvc-op! [addr]
  (when-not (flag-set? FLAG_OVERFLOW)
    (set! *pc* (+ *pc* addr))))

; extra cycles: (+1 if branch succeeds +2 if to a new page)
(defn bvs-op! [addr]
  (when (flag-set? FLAG_OVERFLOW)
    (set! *pc* (+ *pc* addr))))

(defn clc-op! [addr]
  (clear-flag! FLAG_CARRY))

(defn cld-op! [addr]
  (clear-flag! FLAG_DECIMAL))

(defn cli-op! [addr]
  (clear-flag! FLAG_INTERRUPT))

(defn clv-op! [addr]
  (clear-flag! FLAG_OVERFLOW))

(defn- cmp-val! [addr val]
  (let [m (read-mem addr)]
    (if (>= val m)
      (set-flag! FLAG_CARRY)
      (clear-flag! FLAG_CARRY))
    (if (== val m)
      (set-flag! FLAG_ZERO)
      (clear-flag! FLAG_ZERO))
    (calc-sign! (- val m))))

; penalty: 1 cycle?
(defn cmp-op! [addr]
  (cmp-val! addr *acc*))

(defn cpx-op! [addr]
  (cmp-val! addr *xreg*))

(defn cpy-op! [addr]
  (cmp-val! addr *yreg*))

(defn- dec-val! [val]
  (let [res (dec val)]
    (calc-sign! res)
    (calc-zero! res)
    (bit-and res 0xFF)))

(defn dec-op! [addr]
  (write-mem! addr (dec-val! (read-mem addr))))

(defn dex-op! [addr]
  (set! *xreg* (dec-val! *xreg*)))

(defn dey-op! [addr]
  (set! *yreg* (dec-val! *yreg*)))

; penalty: 1 cycle?
(defn eor-op! [addr]
  (let [val (read-mem addr)
        res (bit-xor *acc* val)]
    (calc-sign! res)
    (calc-zero! res)
    (set! *acc* res)))

(defn- inc-val! [val]
  (let [res (inc val)]
    (calc-sign! res)
    (calc-zero! res)
    (bit-and res 0xFF)))

(defn inc-op! [addr]
  (write-mem! addr (inc-val! (read-mem addr))))

(defn inx-op! [addr]
  (set! *xreg* (inc-val! *xreg*)))

(defn iny-op! [addr]
  (set! *yreg* (inc-val! *yreg*)))

(defn jmp-op! [addr]
  (set! *pc* addr))

(defn jsr-op! [addr]
  (push-word-stack! (dec *pc*))
  (jmp-op! addr))


(defn- load-op! [addr]
  (let [val (read-mem addr)]
    (calc-sign! val)
    (calc-zero! val)
    val))

(defn lda-op! [addr]
  (set! *acc* (load-op! addr)))

(defn ldx-op! [addr]
  (set! *xreg* (load-op! addr)))

(defn ldy-op! [addr]
  (set! *yreg* (load-op! addr)))


(defn- lsr-val! [val]
  (let [res (bit-shift-right val 1)]
    (if (> (bit-and val 1) 0)
      (set-flag! FLAG_CARRY)
      (clear-flag! FLAG_CARRY))
    (calc-sign! res)
    (calc-zero! res)
    (bit-and res 0xFF)))

(defn lsr-op! [addr]
  (write-mem! addr (lsr-val! (read-mem addr))))

(defn lsr-acc-op! [addr]
  (set! *acc* (lsr-val! *acc*)))

(defn nop-op! [addr]
  (js/console.log "UNKNOWN OPERATION AT " (.toString (dec *pc*) 16)))

(defn ora-op! [addr]
  (let [val (bit-or *acc* (read-mem addr))]
    (calc-sign! val)
    (calc-zero! val)
    (set! *acc* val)))

(defn pha-op! [addr]
  (push-stack! *acc*))

(defn php-op! [addr]
  (push-stack! (bit-or *status* FLAG_BREAK)))

(defn pla-op! [addr]
  (set! *acc* (pull-stack!))
  (calc-sign! *acc*)
  (calc-zero! *acc*))

(defn plp-op! [addr]
  (set! *status* (bit-or (bit-and-not (pull-stack!) FLAG_BREAK) FLAG_CONTANT)))


(defn- rol-val! [val]
  (let [s (bit-shift-left val 1)
        res (bit-or s (if (flag-set? FLAG_CARRY) 1 0))]
    (calc-sign! res)
    (calc-zero! res)
    (calc-carry! res)
    (bit-and res 0xFF)))

(defn rol-acc-op! [addr]
    (set! *acc* (rol-val! *acc*)))

(defn rol-op! [addr]
  (write-mem! addr (rol-val! (read-mem addr))))

(defn- ror-val! [val]
  (let [s (bit-shift-right val 1)
        res (bit-or s (if (flag-set? FLAG_CARRY) 2r10000000 0))]
    (calc-sign! res)
    (calc-zero! res)
    (if (zero? (bit-and val 1))
      (clear-flag! FLAG_CARRY)
      (set-flag! FLAG_CARRY))
    (bit-and res 0xFF)))

(defn ror-acc-op! [addr]
  (set! *acc* (ror-val! *acc*)))

(defn ror-op! [addr]
  (write-mem! addr (ror-val! (read-mem addr))))

(defn rti-op! [addr]
  (set! *status* (pull-stack!))
  (set! *pc* (pull-word-stack!)))

(defn rts-op! [addr]
  (set! *pc* (inc (pull-word-stack!))))

(defn sbc-op! [addr]
  (adc-val! (bit-xor (read-mem addr) 0xFF)))

(defn sec-op! [addr]
  (set-flag! FLAG_CARRY))

(defn sed-op! [addr]
  (flag-set? FLAG_DECIMAL))

(defn sei-op! [addr]
  (flag-set? FLAG_INTERRUPT))

(defn sta-op! [addr]
  (write-mem! addr *acc*))

(defn stx-op! [addr]
  (write-mem! addr *xreg*))

(defn sty-op! [addr]
  (write-mem! addr *yreg*))

(defn tax-op! [addr]
  (set! *xreg* *acc*)
  (calc-sign! *xreg*)
  (calc-zero! *xreg*))

(defn tay-op! [addr]
  (set! *yreg* *acc*)
  (calc-sign! *yreg*)
  (calc-zero! *yreg*))

(defn tsx-op! [addr]
  (set! *xreg* *sp*)
  (calc-sign! *xreg*)
  (calc-zero! *xreg*))

(defn txa-op! [addr]
  (set! *acc* *xreg*)
  (calc-sign! *acc*)
  (calc-zero! *acc*))

(defn txs-op! [addr]
  (set! *sp* *xreg*))

(defn tya-op! [addr]
  (set! *acc* *yreg*)
  (calc-sign! *acc*)
  (calc-zero! *acc*))

;; -------------------------------------------------
;; OPCODES INDEX
;; -------------------------------------------------
;; FORMAT: code operation-fn addr-fn bytes cycles flags

(def index
  (clj->js
    [[0x00 brk-op! imp-addr 1 7 "czidbvn"]
     [0x01 ora-op! xind-addr 2 6 "cZidbvN"]
     [0x02 nop-op! imp-addr 1 2 "czidbvn"]
     [0x03 nop-op! imp-addr 1 2 "czidbvn"]
     [0x04 nop-op! imp-addr 1 2 "czidbvn"]
     [0x05 ora-op! zp-addr 2 3 "cZidbvN"]
     [0x06 asl-op! zp-addr 2 5 "CZidbvN"]
     [0x07 nop-op! imp-addr 1 2 "czidbvn"]
     [0x08 php-op! imp-addr 1 3 "czidbvn"]
     [0x09 ora-op! imm-addr 2 2 "cZidbvN"]
     [0x0A asl-acc-op! acc-addr 1 2 "CZidbvN"]
     [0x0B nop-op! imp-addr 1 2 "czidbvn"]
     [0x0C nop-op! imp-addr 1 2 "czidbvn"]
     [0x0D ora-op! abs-addr 3 4 "cZidbvN"]
     [0x0E asl-op! abs-addr 3 6 "CZidbvN"]
     [0x0F nop-op! imp-addr 1 2 "czidbvn"]
     [0x10 bpl-op! rel-addr 2 2 "czidbvn"]
     [0x11 ora-op! indy-addr 2 5 "cZidbvN"]
     [0x12 nop-op! imp-addr 1 2 "czidbvn"]
     [0x13 nop-op! imp-addr 1 2 "czidbvn"]
     [0x14 nop-op! imp-addr 1 2 "czidbvn"]
     [0x15 ora-op! zpx-addr 2 4 "cZidbvN"]
     [0x16 asl-op! zpx-addr 2 6 "CZidbvN"]
     [0x17 nop-op! imp-addr 1 2 "czidbvn"]
     [0x18 clc-op! imp-addr 1 2 "Czidbvn"]
     [0x19 ora-op! absy-addr 3 4 "cZidbvN"]
     [0x1A nop-op! imp-addr 1 2 "czidbvn"]
     [0x1B nop-op! imp-addr 1 2 "czidbvn"]
     [0x1C nop-op! imp-addr 1 2 "czidbvn"]
     [0x1D ora-op! absx-addr 3 4 "cZidbvN"]
     [0x1E asl-op! absx-addr 3 7 "CZidbvN"]
     [0x1F nop-op! imp-addr 1 2 "czidbvn"]
     [0x20 jsr-op! abs-addr 3 6 "czidbvn"]
     [0x21 and-op! xind-addr 2 6 "cZidbvN"]
     [0x22 nop-op! imp-addr 1 2 "czidbvn"]
     [0x23 nop-op! imp-addr 1 2 "czidbvn"]
     [0x24 bit-op! zp-addr 2 3 "cZidbVN"]
     [0x25 and-op! zp-addr 2 3 "cZidbvN"]
     [0x26 rol-op! zp-addr 2 5 "CZidbvN"]
     [0x27 nop-op! imp-addr 1 2 "czidbvn"]
     [0x28 plp-op! imp-addr 1 4 "CZIDBVN"]
     [0x29 and-op! imm-addr 2 2 "cZidbvN"]
     [0x2A rol-acc-op! acc-addr 1 2 "CZidbvN"]
     [0x2B nop-op! imp-addr 1 2 "czidbvn"]
     [0x2C bit-op! abs-addr 3 4 "cZidbVN"]
     [0x2D and-op! abs-addr 3 4 "cZidbvN"]
     [0x2E rol-op! abs-addr 3 6 "CZidbvN"]
     [0x2F nop-op! imp-addr 1 2 "czidbvn"]
     [0x30 bmi-op! rel-addr 2 2 "czidbvn"]
     [0x31 and-op! indy-addr 2 5 "cZidbvN"]
     [0x32 nop-op! imp-addr 1 2 "czidbvn"]
     [0x33 nop-op! imp-addr 1 2 "czidbvn"]
     [0x34 nop-op! imp-addr 1 2 "czidbvn"]
     [0x35 and-op! zpx-addr 2 4 "cZidbvN"]
     [0x36 rol-op! zpx-addr 2 6 "CZidbvN"]
     [0x37 nop-op! imp-addr 1 2 "czidbvn"]
     [0x38 sec-op! imp-addr 1 2 "Czidbvn"]
     [0x39 and-op! absy-addr 3 4 "cZidbvN"]
     [0x3A nop-op! imp-addr 1 2 "czidbvn"]
     [0x3B nop-op! imp-addr 1 2 "czidbvn"]
     [0x3C nop-op! imp-addr 1 2 "czidbvn"]
     [0x3D and-op! absx-addr 3 4 "cZidbvN"]
     [0x3E rol-op! absx-addr 3 7 "CZidbvN"]
     [0x3F nop-op! imp-addr 1 2 "czidbvn"]
     [0x40 rti-op! imp-addr 1 6 "czidbvn"]
     [0x41 eor-op! xind-addr 2 6 "cZidbvN"]
     [0x42 nop-op! imp-addr 1 2 "czidbvn"]
     [0x43 nop-op! imp-addr 1 2 "czidbvn"]
     [0x44 nop-op! imp-addr 1 2 "czidbvn"]
     [0x45 eor-op! zp-addr 2 3 "cZidbvN"]
     [0x46 lsr-op! zp-addr 2 5 "CZidbvN"]
     [0x47 nop-op! imp-addr 1 2 "czidbvn"]
     [0x48 pha-op! imp-addr 1 3 "czidbvn"]
     [0x49 eor-op! imm-addr 2 2 "cZidbvN"]
     [0x4A lsr-acc-op! acc-addr 1 2 "CZidbvN"]
     [0x4B nop-op! imp-addr 1 2 "czidbvn"]
     [0x4C jmp-op! abs-addr 3 3 "czidbvn"]
     [0x4D eor-op! abs-addr 3 4 "cZidbvN"]
     [0x4E lsr-op! abs-addr 3 6 "CZidbvN"]
     [0x4F nop-op! imp-addr 1 2 "czidbvn"]
     [0x50 bvc-op! rel-addr 2 2 "czidbvn"]
     [0x51 eor-op! indy-addr 2 5 "cZidbvN"]
     [0x52 nop-op! imp-addr 1 2 "czidbvn"]
     [0x53 nop-op! imp-addr 1 2 "czidbvn"]
     [0x54 nop-op! imp-addr 1 2 "czidbvn"]
     [0x55 eor-op! zpx-addr 2 4 "cZidbvN"]
     [0x56 lsr-op! zpx-addr 2 6 "CZidbvN"]
     [0x57 nop-op! imp-addr 1 2 "czidbvn"]
     [0x58 cli-op! imp-addr 1 2 "czIdbvn"]
     [0x59 eor-op! absy-addr 3 4 "cZidbvN"]
     [0x5A nop-op! imp-addr 1 2 "czidbvn"]
     [0x5B nop-op! imp-addr 1 2 "czidbvn"]
     [0x5C nop-op! imp-addr 1 2 "czidbvn"]
     [0x5D eor-op! absx-addr 3 4 "cZidbvN"]
     [0x5E lsr-op! absx-addr 3 7 "CZidbvN"]
     [0x5F nop-op! imp-addr 1 2 "czidbvn"]
     [0x60 rts-op! imp-addr 1 6 "czidbvn"]
     [0x61 adc-op! xind-addr 2 6 "CZidbVN"]
     [0x62 nop-op! imp-addr 1 2 "czidbvn"]
     [0x63 nop-op! imp-addr 1 2 "czidbvn"]
     [0x64 nop-op! imp-addr 1 2 "czidbvn"]
     [0x65 adc-op! zp-addr 2 3 "CZidbVN"]
     [0x66 ror-op! zp-addr 2 5 "CZidbvN"]
     [0x67 nop-op! imp-addr 1 2 "czidbvn"]
     [0x68 pla-op! imp-addr 1 4 "cZidbvN"]
     [0x69 adc-op! imm-addr 2 2 "CZidbVN"]
     [0x6A ror-acc-op! acc-addr 1 2 "CZidbvN"]
     [0x6B nop-op! imp-addr 1 2 "czidbvn"]
     [0x6C jmp-op! ind-addr 3 5 "czidbvn"]
     [0x6D adc-op! abs-addr 3 4 "CZidbVN"]
     [0x6E ror-op! absx-addr 3 7 "CZidbvN"]
     [0x6F nop-op! imp-addr 1 2 "czidbvn"]
     [0x70 bvs-op! rel-addr 2 2 "czidbvn"]
     [0x71 adc-op! indy-addr 2 5 "CZidbVN"]
     [0x72 nop-op! imp-addr 1 2 "czidbvn"]
     [0x73 nop-op! imp-addr 1 2 "czidbvn"]
     [0x74 nop-op! imp-addr 1 2 "czidbvn"]
     [0x75 adc-op! zpx-addr 2 4 "CZidbVN"]
     [0x76 ror-op! zpx-addr 2 6 "CZidbvN"]
     [0x77 nop-op! imp-addr 1 2 "czidbvn"]
     [0x78 sei-op! imp-addr 1 2 "czIdbvn"]
     [0x79 adc-op! absy-addr 3 4 "CZidbVN"]
     [0x7A nop-op! imp-addr 1 2 "czidbvn"]
     [0x7B nop-op! imp-addr 1 2 "czidbvn"]
     [0x7C nop-op! imp-addr 1 2 "czidbvn"]
     [0x7D adc-op! absx-addr 3 4 "CZidbVN"]
     [0x7E ror-op! abs-addr 3 6 "CZidbvN"]
     [0x7F nop-op! imp-addr 1 2 "czidbvn"]
     [0x80 nop-op! imp-addr 1 2 "czidbvn"]
     [0x81 sta-op! xind-addr 2 6 "czidbvn"]
     [0x82 nop-op! imp-addr 1 2 "czidbvn"]
     [0x83 nop-op! imp-addr 1 2 "czidbvn"]
     [0x84 sty-op! zp-addr 2 3 "czidbvn"]
     [0x85 sta-op! zp-addr 2 3 "czidbvn"]
     [0x86 stx-op! zp-addr 2 3 "czidbvn"]
     [0x87 nop-op! imp-addr 1 2 "czidbvn"]
     [0x88 dey-op! imp-addr 1 2 "cZidbvN"]
     [0x89 nop-op! imp-addr 1 2 "czidbvn"]
     [0x8A txa-op! imp-addr 1 2 "cZidbvN"]
     [0x8B nop-op! imp-addr 1 2 "czidbvn"]
     [0x8C sty-op! abs-addr 3 4 "czidbvn"]
     [0x8D sta-op! abs-addr 3 4 "czidbvn"]
     [0x8E stx-op! abs-addr 3 4 "czidbvn"]
     [0x8F nop-op! imp-addr 1 2 "czidbvn"]
     [0x90 bcc-op! rel-addr 2 2 "czidbvn"]
     [0x91 sta-op! indy-addr 2 6 "czidbvn"]
     [0x92 nop-op! imp-addr 1 2 "czidbvn"]
     [0x93 nop-op! imp-addr 1 2 "czidbvn"]
     [0x94 sty-op! zpx-addr 2 4 "czidbvn"]
     [0x95 sta-op! zpx-addr 2 4 "czidbvn"]
     [0x96 stx-op! zpy-addr 2 4 "czidbvn"]
     [0x97 nop-op! imp-addr 1 2 "czidbvn"]
     [0x98 tya-op! imp-addr 1 2 "cZidbvN"]
     [0x99 sta-op! absy-addr 3 5 "czidbvn"]
     [0x9A txs-op! imp-addr 1 2 "czidbvn"]
     [0x9B nop-op! imp-addr 1 2 "czidbvn"]
     [0x9C nop-op! imp-addr 1 2 "czidbvn"]
     [0x9D sta-op! absx-addr 3 5 "czidbvn"]
     [0x9E nop-op! imp-addr 1 2 "czidbvn"]
     [0x9F nop-op! imp-addr 1 2 "czidbvn"]
     [0xA0 ldy-op! imm-addr 2 2 "cZidbvN"]
     [0xA1 lda-op! xind-addr 2 6 "cZidbvN"]
     [0xA2 ldx-op! imm-addr 2 2 "cZidbvN"]
     [0xA3 nop-op! imp-addr 1 2 "czidbvn"]
     [0xA4 ldy-op! zp-addr 2 3 "cZidbvN"]
     [0xA5 lda-op! zp-addr 2 3 "cZidbvN"]
     [0xA6 ldx-op! zp-addr 2 3 "cZidbvN"]
     [0xA7 nop-op! imp-addr 1 2 "czidbvn"]
     [0xA8 tay-op! imp-addr 1 2 "cZidbvN"]
     [0xA9 lda-op! imm-addr 2 2 "cZidbvN"]
     [0xAA tax-op! imp-addr 1 2 "cZidbvN"]
     [0xAB nop-op! imp-addr 1 2 "czidbvn"]
     [0xAC ldy-op! abs-addr 3 4 "cZidbvN"]
     [0xAD lda-op! abs-addr 3 4 "cZidbvN"]
     [0xAE ldx-op! abs-addr 3 4 "cZidbvN"]
     [0xAF nop-op! imp-addr 1 2 "czidbvn"]
     [0xB0 bcs-op! rel-addr 2 2 "czidbvn"]
     [0xB1 lda-op! indy-addr 2 5 "cZidbvN"]
     [0xB2 nop-op! imp-addr 1 2 "czidbvn"]
     [0xB3 nop-op! imp-addr 1 2 "czidbvn"]
     [0xB4 ldy-op! zpx-addr 2 4 "cZidbvN"]
     [0xB5 lda-op! zpx-addr 2 4 "cZidbvN"]
     [0xB6 ldx-op! zpy-addr 2 4 "cZidbvN"]
     [0xB7 nop-op! imp-addr 1 2 "czidbvn"]
     [0xB8 clv-op! imp-addr 1 2 "czidbVn"]
     [0xB9 lda-op! absy-addr 3 4 "cZidbvN"]
     [0xBA tsx-op! imp-addr 1 2 "cZidbvN"]
     [0xBB nop-op! imp-addr 1 2 "czidbvn"]
     [0xBC ldy-op! absx-addr 3 4 "cZidbvN"]
     [0xBD lda-op! absx-addr 3 4 "cZidbvN"]
     [0xBE ldx-op! absy-addr 3 4 "cZidbvN"]
     [0xBF nop-op! imp-addr 1 2 "czidbvn"]
     [0xC0 cpy-op! imm-addr 2 2 "CZidbvN"]
     [0xC1 cmp-op! xind-addr 2 6 "CZidbvN"]
     [0xC2 nop-op! imp-addr 1 2 "czidbvn"]
     [0xC3 nop-op! imp-addr 1 2 "czidbvn"]
     [0xC4 cpy-op! zp-addr 2 3 "CZidbvN"]
     [0xC5 cmp-op! zp-addr 2 3 "CZidbvN"]
     [0xC6 dec-op! zp-addr 2 5 "cZidbvN"]
     [0xC7 nop-op! imp-addr 1 2 "czidbvn"]
     [0xC8 iny-op! imp-addr 1 2 "cZidbvN"]
     [0xC9 cmp-op! imm-addr 2 2 "CZidbvN"]
     [0xCA dex-op! imp-addr 1 2 "cZidbvN"]
     [0xCB nop-op! imp-addr 1 2 "czidbvn"]
     [0xCC cpy-op! abs-addr 3 4 "CZidbvN"]
     [0xCD cmp-op! abs-addr 3 4 "CZidbvN"]
     [0xCE dec-op! abs-addr 3 6 "cZidbvN"]
     [0xCF nop-op! imp-addr 1 2 "czidbvn"]
     [0xD0 bne-op! rel-addr 2 2 "czidbvn"]
     [0xD1 cmp-op! indy-addr 2 5 "CZidbvN"]
     [0xD2 nop-op! imp-addr 1 2 "czidbvn"]
     [0xD3 nop-op! imp-addr 1 2 "czidbvn"]
     [0xD4 nop-op! imp-addr 1 2 "czidbvn"]
     [0xD5 cmp-op! zpx-addr 2 4 "CZidbvN"]
     [0xD6 dec-op! zpx-addr 2 6 "cZidbvN"]
     [0xD7 nop-op! imp-addr 1 2 "czidbvn"]
     [0xD8 cld-op! imp-addr 1 2 "cziDbvn"]
     [0xD9 cmp-op! absy-addr 3 4 "CZidbvN"]
     [0xDA nop-op! imp-addr 1 2 "czidbvn"]
     [0xDB nop-op! imp-addr 1 2 "czidbvn"]
     [0xDC nop-op! imp-addr 1 2 "czidbvn"]
     [0xDD cmp-op! absx-addr 3 4 "CZidbvN"]
     [0xDE dec-op! absx-addr 3 7 "cZidbvN"]
     [0xDF nop-op! imp-addr 1 2 "czidbvn"]
     [0xE0 cpx-op! imm-addr 2 2 "CZidbvN"]
     [0xE1 sbc-op! xind-addr 2 6 "CZidbVN"]
     [0xE2 nop-op! imp-addr 1 2 "czidbvn"]
     [0xE3 nop-op! imp-addr 1 2 "czidbvn"]
     [0xE4 cpx-op! zp-addr 2 3 "CZidbvN"]
     [0xE5 sbc-op! zp-addr 2 3 "CZidbVN"]
     [0xE6 inc-op! zp-addr 2 5 "cZidbvN"]
     [0xE7 nop-op! imp-addr 1 2 "czidbvn"]
     [0xE8 inx-op! imp-addr 1 2 "cZidbvN"]
     [0xE9 sbc-op! imm-addr 2 2 "CZidbVN"]
     [0xEA nop-op! imp-addr 1 2 "czidbvn"]
     [0xEB nop-op! imp-addr 1 2 "czidbvn"]
     [0xEC cpx-op! abs-addr 3 4 "CZidbvN"]
     [0xED sbc-op! abs-addr 3 4 "CZidbVN"]
     [0xEE inc-op! abs-addr 3 6 "cZidbvN"]
     [0xEF nop-op! imp-addr 1 2 "czidbvn"]
     [0xF0 beq-op! rel-addr 2 2 "czidbvn"]
     [0xF1 sbc-op! indy-addr 2 5 "CZidbVN"]
     [0xF2 nop-op! imp-addr 1 2 "czidbvn"]
     [0xF3 nop-op! imp-addr 1 2 "czidbvn"]
     [0xF4 nop-op! imp-addr 1 2 "czidbvn"]
     [0xF5 sbc-op! zpx-addr 2 4 "CZidbVN"]
     [0xF6 inc-op! zpx-addr 2 6 "cZidbvN"]
     [0xF7 nop-op! imp-addr 1 2 "czidbvn"]
     [0xF8 sed-op! imp-addr 1 2 "cziDbvn"]
     [0xF9 sbc-op! absy-addr 3 4 "CZidbVN"]
     [0xFA nop-op! imp-addr 1 2 "czidbvn"]
     [0xFB nop-op! imp-addr 1 2 "czidbvn"]
     [0xFC nop-op! imp-addr 1 2 "czidbvn"]
     [0xFD sbc-op! absx-addr 3 4 "CZidbVN"]
     [0xFE inc-op! absx-addr 3 7 "cZidbvN"]
     [0xFF nop-op! imp-addr 1 2 "czidbvn"]]))


;; Controller

(defn reset-cpu! []
  (set! *acc* 0)
  (set! *xreg* 0)
  (set! *yreg* 0)
  (set! *sp* 0xFD)
  (set! *pc* (read-mem-word 0xFFFC))
  (set! *status* FLAG_CONTANT))

(defn cpu-step! []
  (let [op-info (aget index (read-mem (read-inc-pc!)))
        op-fn (aget op-info 1)
        addr-fn (aget op-info 2)]
    (op-fn (addr-fn))
    (aget op-info 4)))

(defn cpu-exec-cycles! [cycles]
  (when (and (> cycles 0) (not *break?*))
    (recur (- cycles (cpu-step!)))))

(defn print-state []
  (println "A:" (u/str-lpad (.toString *acc* 16) \0 2))
  (println "X:" (u/str-lpad (.toString *xreg* 16) \0 2))
  (println "Y:" (u/str-lpad (.toString *yreg* 16) \0 2))
  (println "SP:" (u/str-lpad (.toString *sp* 16) \0 2))
  (println "PC:" (u/str-lpad (.toString *pc* 16) \0 4))
  (println "NV-BDIZC")
  (println (u/str-lpad (.toString *status* 2) \0 8)))