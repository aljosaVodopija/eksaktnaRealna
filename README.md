#eksaktnaRealna

Repozitorij za projekt Računanje z eksaktnimi realnimi števili (MFP)

Za uporabo je treba paket namestiti s programom Cabal.

## Realna števila
Realna števila so reprezentirana kot presek padajoče verige  zaprtih intervalov, katerih dolžine konvergirajo proti nič. Robovi intervalov so diadična števila.
### Osnovne operacije na realnih številih
```
*Reals> let a=exact 1.3
*Reals> a*(1-a)
[-1635786*2^-22,-1635768*2^-22] -0.3899996280670166
*Reals> a^200
[1705561*2^55,1706556*2^55] 6.146723539897814e22
"ghci> " exact 2.2 < exact 2.7
True
```
### Funkciji `apart` in `equal`
Realna števila tvorijo Hausdorffov prostor, zato lahko definiramo funkcijo `apart`, ki za dve števili preveri ali sta različni. 
```
*Reals> sin (exact 0) `apart` cos (exact 0)
True
```
Realna števila niso diskreten prostor, zato ne moremo definirati funkcije `equal`, ki bi preverila ali sta dve števili enaki. Posledično funkcija `apart` za dve enaki točki ne zna določiti ali sta različni.
### Funkciji `forall` in `exists`
Zaprti intervali so kompaktne množice. Posledično lahko definiramo funkcijo `forall`, ki nam preveri veljavnost predikata na zaprtih intervalih. 

Za poljuben interval lahko definiramo funkcijo `exists`, ki nam preveri ali v tej množici obstaja element, ki zadošča danemu predikatu.
```
*Reals> forall (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `less` exact 0.26
True
*Reals> exists (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `less` exact 0.24
True
*Reals> exists (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `more` exact 0.26
False
```
### Naprednejše funkcije
Implementirana je funkcija `approx_to`, ki za dano realno število vrne približek v obliki diadičnega števila na izbrano natančnost. 
```
*Reals> approx_to (exact 2/3) 60 RoundDown
(3074457345618258602*2^-62,60)
*Reals> 3074457345618258602/2^62
0.6666666666666666
```
Definirana je funkcija `lim`, ki za dano konvergentno zaporedje izračuna njegovo limito. 
```
*Reals> let x k = (exact (1.2+1/2^k), Dyadic{mant=1, expo= -k})
*Reals> lim x
[1258290*2^-20,1258292*2^-20] 1.1999998092651367
```
###Instanca `Floating`
V instanci `Floating` so definirane funkcije `exp`, `log`, `sin`, `cos`, ... in število `pi`.
```
*Reals> exp $ exact 1
[45605199*2^-24,45605204*2^-24] 2.7182818353176117
*Reals> exp 1
2.718281828459045
*Reals> log $ exact 1.5
[425160*2^-20,425163*2^-20] 0.40546560287475586
*Reals> log 1.5
0.4054651081081644
*Reals> sin $ exact 21
[14036750*2^-24,14036755*2^-24] 0.8366556465625763
*Reals> sin 21
0.8366556385360561
*Reals> tan $ exact 21
[-1601701*2^-20,-1601696*2^-20] -1.527498722076416
*Reals> tan 21
-1.5274985276366035
```
## Cela števila
Cela števila z diskretno topologijo tvorijo diskreten, Hausdorffov prostor. Zato lahko na celih številih implementiramo tako funkcijo `apart` kot `equal`. Kompakti na celih številih so končne podmnožice, za katere lahko tako definiramo `forall`.
```
*Integers> 42 `equal` 42
True
*Integers> forall ([1..42]::SubSet) $ \x-> (x^42+33) `apart` 34
False
*Integers> forall ([1..42]::SubSet) $ \x-> (x^42+33) `apart` 42
True
*Integers> exists ([1..]::SubSet) $ \x-> (x^42+33) `less` 42
True
```
