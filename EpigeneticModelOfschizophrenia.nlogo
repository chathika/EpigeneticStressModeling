extensions [ csv ]
breed [people person]
__includes ["PopulationDynamics.nls"]
people-own[
  ;schizophrenic-risk ; boolean
  ;risk-age
  schizophrenia-inherited ; boolean
  schizophrenia-exhibited
  age ;; integer
  ;stressed
  ;;
  father
  mother
  siblings
  children
  sex
  births-this-tick
  birth-region
]
patches-own [region]
globals [;FirstDegreeRR SecondDegreeRR
  year
  month
  number-of-births
  annual-fertility
  annual-mortality
  birth-rate-adjustment
  ;total-schizophrenia-inherited RR-stress-multiplier
]
to setup
  clear-all
  ;set FirstDegreeRR csv:from-file "data/FirstDegreeRR.csv"
  ;set SecondDegreeRR csv:from-file "data/SecondDegreeRR.csv"
  ;set RR-stress-multiplier 1
  createRegions
  set year 1940
  set month 1
  set annual-fertility read-fertility-data
  set annual-mortality read-mortality-data
  init-population
  ;set p1 0.03918391838669777
  ;set p2 33.93622589111328
  ;set p3 -27.331884384155273
  ask turtles [set siblings (list)]

  reset-ticks

end

to createRegions
  let border1 (world-height / 3 - world-height / 2)
  let border2 (2 * world-height / 3 - world-height / 2)
  let _border_size 4
  ask patches with [pycor > border1 - 2] [set region "SouthControl" set pcolor grey + 4 ]
  ask patches with [pycor > border1 + 2 and pycor < border2 - 2] [set region "Famine" set pcolor grey + 4]
  ask patches with [pycor < border1 + 2] [set region "NorthControl" set pcolor grey + 4]
  ask patches with [pycor < _border_size + border1 and pycor > border1 - _border_size] [set pcolor black set region "Border"]
  ask patches with [pycor < _border_size + border2 and pycor > border2 - _border_size] [set pcolor black set region "Border"]
end

;; Observer procedure
to go
  ;;;;; Population dynamics
  runPopulationDynamics
  move-around
  exhibit-schizophrenia
  ;;;;;
  tick
  update-time-and-age
  if (year = stop-year or not any? People) [stop]
end

;; Observer procedure
to update-time-and-age
  ifelse month < 12 [set month month + 1][
    set month 1
    set year year + 1
    ask people [
      set age age + 1
    ]
  ]
end



to move-around
  ;;;;; Random walk
  ask people [
    ;move-to one-of patches with [region = [birth-region] of myself]
    set heading random 360
    if [region] of patch-ahead 1 = birth-region [fd 1]
    ;let current-region [region] of patch-here
    ;while [[region] of patch-ahead 1 = current-region][fd 1]
  ]
end
to-report init-new-person [m f]
  set xcor random world-width
  set ycor random world-height
  ;set shape "face happy"
  set mother m
  set father f
  let _child_region [region] of one-of patches with [region != "Border"]
  if mother != nobody and father != nobody [
    let _mothers_patch [region] of (patch-at [xcor] of mother [ycor] of mother)
    let _fathers_patch [region] of (patch-at [xcor] of mother [ycor] of father)
    if _mothers_patch != _fathers_patch [
     ;error "Mother and Father cannot be from two different regions!"
    ]
    set _child_region _mothers_patch
  ]
  set birth-region _child_region
  move-to one-of patches with [region = _child_region ]
  ;set color ifelse-value ([region] of patch-here = "SouthControl") [red] [ifelse-value ([region] of patch-here = "NorthControl") [green][blue]]
  ;set stressed false
  ;set schizophrenic-risk false
  ;set schizophrenia false
  set children (list)
  set sex ifelse-value (random-float 1 < 0.5) ["female"]["male"]
  if mother = nobody or father = nobody [
    set schizophrenia-inherited ifelse-value (random-float 1 < (init-percent-schizophrenia-inherited / 100)) [true][false]
    set schizophrenia-exhibited ifelse-value (schizophrenia-inherited and (random-float 1 < (init-percent-schizophrenia-exhibited / 100))) [true][false]
  ]
  set color ifelse-value (schizophrenia-exhibited) [red][ifelse-value(schizophrenia-inherited)[yellow][green]]
  set shape ifelse-value (schizophrenia-exhibited) ["face sad"][ifelse-value(schizophrenia-inherited)["face neutral"]["face happy"]]
  ;inherit-schizophrenic-risk
  set size 5
  report who
end



;; people develop schizophrenia like this
;to inherit-schizophrenic-risk
;  ;; Check first degree
;  let RR 1
;  if father != 0 and father != nobody and [schizophrenia] of father [
;    set RR ifelse-value  (sex = "male") [ item 5 (item 1 FirstDegreeRR)][RR]
;    set RR ifelse-value  (sex = "female") [ item 5 (item 2 FirstDegreeRR)][RR]
;  ]
;  if mother != 0 and mother != nobody and [schizophrenia] of mother [
;    set RR ifelse-value  (sex = "male") [ item 5 (item 3 FirstDegreeRR)][RR]
;    set RR ifelse-value  (sex = "female") [ item 5 (item 4 FirstDegreeRR)][RR]
;  ]
;  if mother != 0 and mother != nobody and [schizophrenia] of mother and father != 0 and father != nobody and [schizophrenia] of father [
;    set RR 14.66
;  ]
;  if stressed [
;    set RR RR * RR-stress-multiplier
;  ]
;  let probSchizo base-schizophrenia-inheritability * RR
;  if random-float 1 < probSchizo [
;   set schizophrenic-risk true
;    set color yellow
;    set risk-age age-to-exhibit-schizophrenia
;    set total-schizophrenia-inherited total-schizophrenia-inherited + 1
;  ]
;  ;if random-float 1 < stressed * ifelse-value epigenetic-risk [relative-risk-of-manifestation][1] [
;   ;set schizophrenia true
;   ;set color red
;  ;]
;end
;to-report age-to-exhibit-schizophrenia
;  let age-to-exhibit 0
;  set age-to-exhibit ifelse-value (sex = "female" ) [random-normal 41.9 19.5] [age-to-exhibit] ; Chou et al , 2017
;  set age-to-exhibit ifelse-value (sex = "male" ) [random-normal 41.2 19.7] [age-to-exhibit] ;
;  report age-to-exhibit
;end
;to Expose-to-Stress
;  set p1 0.16069729626178741
;  set p2 -22.134449005126953
;  set p3 -69.08554077148438
;  set RR-stress-multiplier 2 ; Famine paper
;  ask people [
;    set stressed true
;
;  ]
;end

to init-population
  let NetherlandsInitialPopulationAges csv:from-file "NetherlandsInitialPopulationAges.csv"
  set NetherlandsInitialPopulationAges but-first NetherlandsInitialPopulationAges
  let total-pop 0
  foreach NetherlandsInitialPopulationAges [age-group ->
    set total-pop total-pop + (item 2 age-group )
  ]
  let scale-factor 1000 / scale
  foreach NetherlandsInitialPopulationAges [age-group ->
    let group-size (item 2 age-group) * scale-factor
    create-people group-size[
      let id init-new-person nobody nobody
      ask person id [set age  item 0 age-group + (random (item 1 age-group - item 0 age-group))]

    ]

  ]
  ;; assign parents assuming
  ask people with [mother = nobody or father = nobody][
     let _potential-mothers other people with [sex = "female" and age > 16 + [age] of myself]
     set mother one-of _potential-mothers ;with [(random-float 1 < fertility / (1000 * 12))]
     let _potential-fathers other people with [sex = "male" and age > 16 + [age] of myself]
     set father one-of _potential-mothers ;with [(random-float 1 < fertility / (1000 * 12))]

   let id init-new-person mother father
  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
161
11
571
622
-1
-1
2.0
1
10
1
1
1
0
1
1
1
-100
100
-150
150
0
0
1
ticks
30.0

BUTTON
61
66
124
99
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
61
102
124
135
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
158
871
368
904
relative-risk-of-manifestation
relative-risk-of-manifestation
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
153
836
380
869
base-schizophrenia-inheritability
base-schizophrenia-inheritability
0
100
0.006
1
1
NIL
HORIZONTAL

PLOT
605
14
968
211
Number of Children
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [length children ] of  turtles"
"pen-1" 1.0 0 -7500403 true "" "plot max [length children ] of  turtles"
"pen-2" 1.0 0 -9276814 true "" "plot min [length children ] of  turtles"

PLOT
980
18
1198
187
Entire Population
NIL
NIL
0.0
10.0
7000.0
10000.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "plot count turtles"

PLOT
987
190
1187
340
Births
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "plot sum [births-this-tick] of people"

PLOT
1204
19
1404
169
Age Distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [age] of people"
"pen-1" 1.0 0 -7500403 true "" "plot max [age] of people"
"pen-2" 1.0 0 -9276814 true "" "plot min [age] of people"

SLIDER
648
248
820
281
scale
scale
0
10000
5000.0
1
1
th
HORIZONTAL

MONITOR
16
153
73
198
NIL
year
17
1
11

MONITOR
73
153
130
198
NIL
month
17
1
11

SLIDER
610
305
872
338
init-percent-schizophrenia-inherited
init-percent-schizophrenia-inherited
0
100
1.0
1
1
%
HORIZONTAL

SLIDER
610
340
872
373
init-percent-schizophrenia-exhibited
init-percent-schizophrenia-exhibited
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
591
531
878
564
schizophrenia-inherit-threshold
schizophrenia-inherit-threshold
0
1
0.001
0.0001
1
NIL
HORIZONTAL

SLIDER
589
567
877
600
schizophrenia-via-social-interaction-threshold
schizophrenia-via-social-interaction-threshold
0
50
0.0
1
1
NIL
HORIZONTAL

SLIDER
589
612
878
645
schizophrenia-exhibit-inherited-threshold
schizophrenia-exhibit-inherited-threshold
0
1
1.0E-4
0.001
1
NIL
HORIZONTAL

SLIDER
592
651
893
684
schizophrenia-exhibit-uninherited-threshold
schizophrenia-exhibit-uninherited-threshold
0
1
1.0E-4
0.001
1
NIL
HORIZONTAL

PLOT
905
373
1419
592
Schizophrenia Exhibited by Region
Time
Count
0.0
10.0
0.0
5.0
true
true
"" ""
PENS
"NorthControl" 1.0 0 -13345367 true "" "plot 100 * count people with [region = \"NorthControl\" and schizophrenia-exhibited] / count people"
"SouthControl" 1.0 0 -14439633 true "" "plot 100 * count people with [region = \"SouthControl\" and schizophrenia-exhibited] / count people"
"Famine" 1.0 0 -2674135 true "" "plot 100 * count people with [region = \"Famine\" and schizophrenia-exhibited] / count people"

INPUTBOX
13
197
73
257
stop-year
1970.0
1
0
Number

TEXTBOX
697
285
847
303
Initial Conditions
14
0.0
1

TEXTBOX
679
501
829
519
Model Parameters
14
0.0
1

PLOT
904
600
1419
750
RR
Time
RR
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"RRParents" 1.0 0 -16777216 true "" "plot ifelse-value (count people <= 0)[0][meanRRParents]"
"RRSiblings" 1.0 0 -4079321 true "" "plot ifelse-value (count people <= 0)[0][meanRRSiblings]"

INPUTBOX
93
670
189
730
stress-start-year
1945.0
1
0
Number

INPUTBOX
189
670
344
730
stress-start-month
10.0
1
0
Number

INPUTBOX
306
670
393
730
stress-end-year
1945.0
1
0
Number

INPUTBOX
393
670
486
730
stress-end-month
12.0
1
0
Number

SLIDER
584
684
878
717
inheretence-augmentation-by-shock
inheretence-augmentation-by-shock
0
100
50.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
