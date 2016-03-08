;;; -*- lisp -*-
(defpackage turtar/humidity
  (:use :cl :oliphaunt
        :turtar/entity)
  (:export #:humidity))
(in-package :turtar/humidity)

;;; Humidity
;;; 
;;; Part of Turtar
;;; 
;;; Copyright © 2016, Bruce-Robert Fenn Pocock
;;;
;;; This program is free  software: you can redistribute it and/or  modify it under the terms of  the GNU Affero General
;;; Public License as  published by the Free Software Foundation,  either version 3 of the License,  or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in  the hope that it will be useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of  MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU  Affero General Public  License for
;;; more details.
;;; 
;;; You should  have received a  copy of  the GNU Affero  General Public License  along with  this program. If  not, see
;;; <http://www.gnu.org/licenses/>.

(defclass humidity (component)
  ((litres-water :initarg :litres-water :reader litres-water)
   (water-temperature :initarg :water-temperature :reader water-temperature)
   (droplet-size :initarg :droplet-size :reader droplet-size)
   (condensation-particulates :initarg :condensation-particulates :reader condensation-particulates)
   (wind-vector :initarg :wind-vector :reader wind-vector)))

;;; Reference: (see  doc/reference folder  “Vapour Pressures  of ice  and supercooled  water”) Q.  J. R.  Meteorol. Soc.
;;; (2005), 131, pp.  1539–1565 doi: 10.1256/qj.04.94 “Review of  the vapour pressures of ice and  supercooled water for
;;; atmospheric  applications” D.  M.  MURPHY (Aeronomy  Laboratory, National  Oceanic  and Atmospheric  Administration,
;;; Boulder, USA) and T. KOOP (University of Bielefeld, Faculty of Chemistry, Germany)

(defconstant +e*_st+ 1013.25	"e* at the steam-point pressure (1 atm = 1013.25 hPa)")
(defconstant +e*_i0+ 6.1173	"e* at the ice-point pressure (in hPa)")
(defconstant +T₀+ 273.16	"triple-point of water in Kelvin")
(defconstant +T₀/°C+ 0.01	"triple-point of water in °C")
(defconstant +T_st+ 373.15	"steam point of water in Kelvin (boiling point at 1 atm)")

(defconstant +atm+ 1013.25	"Normal atmosphric pressure in hPa at 0°C")

(defconstant +hPa->lb/in²+ .0145038	"hPa → Pounds/inch² (PSI)")

(defun °C->K (n°C) "Convert °C to Kelvin."	(+ n°C (- +T₀/°C+) +T₀+))
(defun K->°C (nK) "Convert Kelvin to °C."	(+ nK +T₀/°C+ (- +T₀+)))

(defun temp->saturation-vapor-pressure (temp)
  (check-type temp (real 184 373.15))
  (if (>= temp +T₀+)
      (temp->saturation-vapor-pressure/water temp)
      (temp->saturation-vapor-pressure/ice temp)))

(defun temp->saturation-vapor-pressure/ice (temp)
  "Compute the saturation water vapour pressure in hPa over ice.
 
Based upon the Goff-Gratch 1946 edition, where:

$\\log\\ e^*_i\\ = -9.09718(T_0/T-1)\\ -\\ 3.56654\\ \\log(T_0/T)+\\ 0.876793(1-T/T_0) +\\ \\log\\ e^*_{i0}$"
  (check-type temp (real 184 274))
  (assert (<= temp +T₀+))
  (let ((T₀/temp (/ +T₀+ temp)))
    (expt 10 (+ (* -9.09718 (1- T₀/temp))
                (* -3.56654 (log T₀/temp 10))
                (* .876793 (- 1 (/ temp +T₀+)))
                (log +e*_i0+ 10)))))

(defconstant +euler+ 2.7182818284590452353602875	"Euler's constant “e”")

(defun temp->saturation-vapor-pressure/water (temp)
  "Compute the saturation water  vapour pressure in hPa over water.  Based on Sonntag 1990, as this  was the first model
I found that yielded reasonable fit to the Appendix C confirmation values (p. 1561) in Murphy Koop 2005."
  (check-type temp (real 173.15 373.15))
  (expt +euler+ (+ 16.635764
                   (/ -6096.9385 temp)
                   (* -2.711193 (expt 10 -2) temp)
                   (* 1.673952 (expt 10 -5) (expt temp 2))
                   (* 2.433502 (log temp))) )
  #+goff-gratch
  (let ((T_st/temp (/ +T_st+ temp))
        (temp/T_st (/ temp +T_st+)))
    "Compute the saturation water vapour pressure in hPa. Based upon the Goff-Gratch 1946 edition, where:

$\\log\\       e^*\\       =       -7.90298(T_\\mathrm{st}/T-1)\\       +\\       5.02808\\       \\log(T_\\mathrm{st}/T)       -
 1.3816\\times10^{-7}(10^{11.344(1-T/T_\\mathrm{st})}-1)  +\\  8.1328\\times10^{-3}(10^{-3.49149(T_\\mathrm{st}/T-1)}-1)\\  +\\
 \\log\\ e^*_\\mathrm{st}$
"
    (check-type temp (real 273.16 373.15))
    (expt 10 (+ (* -7.90298 (- T_st/temp 1))
                (* 5.02808 (log T_st/temp 10))
                (* -1.3816 (expt 10 -7) (- (expt 10 (* 11.344 (- 1 temp/T_st))) 1))
                (* 8.1328 (expt 10 -3) (- (expt 10 (* -3.49149 (- temp/T_st 1))) 1))
                (log +e*_st+ 10))))
  #+goff-1957
  (let ((T₀/temp (/ +T₀+ temp))
        (temp/T₀ (/ temp +T₀+)))
    "Goff (1957)"
    (check-type temp (real 273.15 373.15))
    (+ (log 611.14 10)
       (* 10.79574 (1- T₀/temp))
       (* -5.0280 (log temp/T₀ 10))
       (* 1.50475 (expt 10 -4) (- 1 (expt 10 (* -8.2969 (1- temp/T₀)))))
       (* .42873 (expt 10 -3) (1- (expt 10 (* 4.76955 (- 1 T₀/temp)))))))
  #+goff-1963
  (let ()
    "Goff-1963"
    (check-type temp (real 273.15 743.15))
    (+ (log 611.11 10)
       (* 10.79586 (- 1 (/ +T₀+ temp)))
       (* -5.02808 (log (/ temp +T₀+) 10))
       (* 1.50474 (expt 10 -4) (- 1 (expt 10 (* 4.76955 (- 1 (/ +T₀+ temp))))))
       (* .42873 (expt 10 -3) (1- (expt 10 (* 4.76955 (- 1 (/ +T₀+ temp))))))))
  #+hyland-wexler-1983
  (let ()
    "Hyland & Wexler 1983"
    (check-type temp (real 273.15 473.15))
    (+ (/ -5800.2206 temp)
       1.3914993
       (* -.48640239 (expt 10 -1) temp)
       (* .41764768 (expt 10 -4) (expt temp 2))
       (* -.14452093 (expt 10 -7) (expt temp 3))
       (* 6.5459673 (log temp)))))


(defun least-magnitude (value)
  (expt 10 (1+ (- (round (log value 10)) 
                  (length (string-trim "-0." (princ-to-string value)))))))

(defun ≈ (a b &key (ε (min (least-magnitude a)
                           (least-magnitude b))))
    (< (abs (- a b)) ε))


;;; Assertions from doi: 10.1256/qj.04.94 Appendix C

;; These are outside the range supported by the above ƒs
;; (assert (= (temp->saturation-vapor-pressure 150) .000006106))
;; (assert (= (temp->saturation-vapor-pressure 180) .0053975))
(assert (≈ (temp->saturation-vapor-pressure 210) .0070202 :ε .0005))
(assert (≈ (temp->saturation-vapor-pressure 240) .27272 :ε .0005))
(assert (≈ (temp->saturation-vapor-pressure 273.15) 6.11213 :ε .0005))
(assert (≈ (temp->saturation-vapor-pressure 273.16) 6.11657 :ε .0005))
(assert (≈ (temp->saturation-vapor-pressure 300) 35.368 :ε .002))

(defconstant +H_vap+ 2257	"Enthalpy of vaporization of water = 2257 kJ/kg")
(defconstant +Avogadro+ (* 6.02214085774 (expt 10 23)))
(defconstant +Boltzmann+ (* 1.3806485279 (expt 10 -23)))
(defconstant +R+ (* +Avogadro+ +Boltzmann+)	"Gas Constant (~8.314462175 J⋅K¯¹mol¯¹)")

(defconstant +Θ/h+ (+ 25 19)	"Evaporation constant (in kg/m²h)")
(defconstant +Θ/s+ (/ +Θ/h+ (* 60 60))	"Evaporation constant (in kg/m²s)")

(defun humidity-ratio (vapor-partial-pressuce temperature)
  (* .62198   ; Questionable magic number. No attribution.
     (/ vapor-partial-pressure 
        (- (temp->saturation-vapor-pressure temperature)
           vapor-partial-pressure))))

(defun latent-heat-of-water-vapor (temperature water-vapor-mass-kg)
  "in kJ/kg"
  (+ (* 1.006                              ; kJ/kg°C — magic number — unattributed
        (K->°C temperature))
     (* water-vapor-mass-kg (+ (* 1.84    ; another unsourced magic number
                                  (K->°C temperature))
                               +H_vap+))))

(defconstant +specific-heat-of-water+ 4.187	"kJ/kgK (very approx.)")

(defun latent-heat-of-water-droplets (temperature water-vapor-mass-kg vapor-partial-pressure)
  (+ (* 1.006
        (K->°C temperature))
     (+ (* (humidity-ratio vapor-partial-pressure temperature)
           1.84 (K->°C temperature))
        +H_vap+
        (* (- (humidity-ratio vapor-partial-pressure
                              temperature)
              (humidity-ratio (temp->saturation-vapor-pressure temperature)
                              temperature ))
           (* (- 4.19
                 +specific-heat-of-water+)
              (K->°C temperature))))))

(defun evaporation (water-body humidity)
  (let* ((x (humidity-ratio (vapor-partial-pressure humidity)
                            (temperature humidity)))
         (x_s (humidity-ratio (temp->saturation-vapor-pressure (temperature humidity))
                              (temperature humidity))))
    
    (let ((evaporated-kg/s (* +Θ/s+ (surface-area water-body) (- x_s x)))
          ;; heat taken in kJ/s = kW

          (heat-taken (* evaporated-kg/s +H_vap+)))
      
      (values evaporated-kg/s heat-taken))))

#|
Latent Heat Flow - SI-Units

The latent heat flow can be expressed in SI-units (metric) as

Ql = hwe ρ q Δx / 3600         (2)

where

Ql = latent heat flow (kW)

hwe = 2465.56 - latent heat of vaporization of water (kJ/kg)

ρ = 1.202* - air density at standard conditions (kg/m3)

q = air flow (m3/hr)

Δx = humidity ratio difference (kg water/kg dry air)

Sensible Heat

|#
