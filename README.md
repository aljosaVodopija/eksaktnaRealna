eksaktnaRealna
==============

Repozitorij za projekt Računanje z eksaktnimi realnimi števili (MFP)

Za uporabo je treba paket namestiti s programom Cabal.

Primeri uporabe:
Prelude> import Reals
*Reals> let a=exact 1.3
Loading package eksaktnaRealna-0.1.0.0 ... linking ... done.
*Reals> a*(1-a)
[-1635786*2^-22,-1635768*2^-22] -0.3899996280670166
*Reals> a^200
[1705561*2^55,1706556*2^55] 6.146723539897814e22 