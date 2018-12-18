! feb-2013 p. marguinaud use jngeom & jnexpl parameters
!                        reallocate cadre when redefinition happens
! oct-2012 p. marguinaud 64b lfi
! jan-2011 p. marguinaud thread-safe fa
subroutine facadi_mt64                                           &
&                   (fa, krep, cdnomc, ktyptr, pslapo, pclopo,   &
&                    pslopo,                                     &
&                    pcodil, ktronc, knlati, knxlon, knlopa,     &
&                    knozpa, psinla, kniver, prefer, pahybr,     &
&                    pbhybr, ldmodc, ldredf, kphase, krangc,     &
&                    klnomc, kgarde)
use fa_mod, only : fa_com, new_cadre, free_cadre, jpniil, jngeom, jnexpl
use parkind1, only : jprb
use yomhook , only : lhook, dr_hook
use lfi_precision
implicit none
!****
!        sous-programme a usage interne au logiciel. fait la plupart
!     des controles en vue de definir un cadre, voire le redefinir.
!        en mode multi-taches, il doit y avoir verrouillage global
!     de la zone d'appel au sous-programme.
!**
!        arguments : krep   ==> code-reponse du sous-programme;
!                    cdnomc ==> nom symbolique du cadre;
!  (tous d'entree)   ktyptr ==> type de transformation horizontale;
!   sauf krangc      pslapo ==> sinus de la latitude du pole d'interet;
!     et klnomc)     pclopo ==> cosinus " " longitude "   "       "   ;
!                    pslopo ==> sinus   " " longitude "   "       "   ;
!                    pcodil ==> coefficient de dilatation;
!                    ktronc ==> troncature;
!                    knlati ==> nombre de latitudes (de pole a pole);
!                    knxlon ==> nombre maxi de longitudes par parallele;
!         (tableau)  knlopa ==> nombre de longitudes par parallele;
!                               (du pole nord vers l'equateur seulement)
!         (tableau)  knozpa ==> nombre d'onde zonal maxi par parallele;
!                               (du pole nord vers l'equateur seulement)
!         (tableau)  psinla ==> sinus des latitudes de l'hemisphere nord
!                               (du pole nord vers l'equateur seulement)
!                    kniver ==> nombre de niveaux verticaux;
!                    prefer ==> pression de reference (facteur multipli-
!                               catif de la premiere fonction de la
!                               coordonnee hybride)
!         (tableau)  pahybr ==> valeurs de la fonction "a" de la coordo-
!                               nnee hybride aux limites de couches;
!         (tableau)  pbhybr ==> valeurs de la fonction "b" de la coordo-
!                               nnee hybride aux limites de couches;
!                    ldmodc ==> vrai s'il y a modification d'un cadre
!                               deja defini au prealable;
!                    ldredf ==> vrai s'il y a redefinition d'un cadre
!                               au sens large du terme (avec ou sans
!                               modification).
!                    kphase ==> indique quelle(s) phase(s) du sous-prog.
!                               on doit executer:
!                               0 ==> toutes,
!                               1 ==> controle des variables simples,
!                               2 ==> controle des tableaux,
!                               3 ==> definition du cadre seule.
!        (sortie)    krangc ==> rang du cadre dans les tables.
! (sortie si phase 1,klnomc ==> longueur en caracteres du nom de cadre.
!  entree sinon ! )
!                    kgarde ==> option de conservation du cadre
!                               apres la fermeture du dernier fichier
!                               qui s'y rattache. a noter que lors dans
!                               le cas d'une definition dynamique de
!                               cadre (appel par faitou, avec kgarde=1),
!                               une redefinition de cadre n'est toleree
!                               qu'a l'identique.
!
!     n.b. :    en mode multi-taches, si l'on appelle le sous-programme
!            avec kphase=0 ou kphase=3, on doit verrouiller dans le
!            programme appelant l'appel au sous-programme.
!               par ailleurs, ldmodc et ldredf ne sont definis que si
!            kphase=0 ou kphase=3.
!*
!        la "redefinition" d'un cadre est possible a l'une de ces
!     conditions:
!
!     - le cadre a ete defini, mais n'a aucun fichier qui s'y rattache;
!     - le cadre defini a au moins un fichier qui s'y rattache, et les
!       nouveaux parametres de definition sont identiques a ceux deja
!       definis (a l'exception de l'option de conservation).
!
!
!
type(fa_com) :: fa
integer (kind=jplikb) ktyptr, ktronc, knlati
integer (kind=jplikb) knxlon, kniver, krep, kphase
integer (kind=jplikb) krangc, klnomc, kgarde
!
integer (kind=jplikb) knlopa (fa%jpxpah), knozpa (fa%jpxind)
!
real (kind=jpdblr) pslapo, pclopo, pslopo, pcodil, prefer
!
real (kind=jpdblr) psinla (fa%jpxgeo), pahybr (0:kniver)
real (kind=jpdblr) pbhybr (0:kniver)
real (kind=jpdblr),parameter ::  zeps=1.e-15_jpdblr
!
character cdnomc*(*)
!
logical ldredf, ldmodc  
!
integer (kind=jplikb) inpahe
integer (kind=jplikb) ilcdno, j, iprec, icompt, imsmax
integer (kind=jplikb) isflam, jl, ik, inimes, inumer, ilnomc
!
integer (kind=jplikb) iesn0 (0:fa%jpxtro)
integer (kind=jplikb) ikntmp(0:fa%jpxtro)
integer (kind=jplikb) ikmtmp(0:fa%jpxtro)
integer (kind=jplikb) icpl4n(0:fa%jpxtro)
!
real (kind=jpdblr) zmin, zpmin, zpmax, zpminp, zpmaxp
!
logical llmlam
character(len=fa%jpxnom) clacti 
character(len=fa%jplmes) clmess 
character(len=fa%jplspx) clnspr
logical                  llfata

type a 
integer x
endtype
!**
!     0.  -  aiguillage en fonction de *kphase*.
!-----------------------------------------------------------------------
!

real(kind=jprb) :: zhook_handle
if (lhook) call dr_hook('facadi_mt',0,zhook_handle)

where(a <=0) a= 0
clacti=''
krep=0
ldredf=.false.
ldmodc=.false.
!
if (ktyptr .le. 0 ) then
   llmlam = .true.
else
   llmlam = .false.
endif
!
inpahe=(1+knlati)/2
!
if (kphase.eq.2) then
  goto 200
elseif (kphase.eq.3) then
  goto 300
elseif (kphase.lt.0.or.kphase.gt.3) then
  krep=-66
  goto 1001
endif
!**
!     1.  -  controle des variables simples (syntaxe et coherence).
!            (sauf pression de reference)
!-----------------------------------------------------------------------
!
ilcdno=int (len (cdnomc), jplikb)
klnomc=1
!
if (ilcdno.le.0) then
  krep=-65
  goto 1001
elseif (cdnomc.eq.' ') then
  krep=-68
  goto 1001
elseif (kgarde.lt.0.or.kgarde.gt.2) then
  krep=-66
  goto 1001
endif
!
do j=ilcdno,1,-1
!
if (cdnomc(j:j).ne.' ') then
  klnomc=j
  goto 102
endif
!
enddo
!
102 continue
!
if (klnomc.gt.fa%ncpcad) then
  krep=-65
  goto 1001
endif
!
if (ktronc.le.0.or.ktronc.gt.fa%nxtron) then
  krep=-70
  goto 1001
elseif (knlati.le.0.or.knlati.gt.fa%nxlati) then
  krep=-71
  goto 1001
elseif (kniver.le.0.or.kniver.gt.fa%nxnivv) then
  krep=-72
  goto 1001
elseif (knxlon.le.0.or.knxlon.gt.fa%nxlong) then
  krep=-83
  goto 1001
endif

if (llmlam) then
!        if (-2*ktyptr+1.gt.knxlon) then
!          krep=-115
!          goto 1001
!        elseif (2*ktronc+1.gt.knlati) then
!          krep=-116
!          goto 1001
!        endif
else
  if (pcodil.lt.1._jpdblr) then
    krep=-73
    goto 1001
  elseif (ktyptr.le.0.or.ktyptr.gt.fa%ntyptx) then
    krep=-109
    goto 1001
  elseif (max(abs(pslapo),abs(pclopo),abs(pslopo)) &
&          .gt.1._jpdblr) then
    krep=-100
    goto 1001
  elseif (abs (1._jpdblr-(pclopo**2+pslopo**2)) &
&          .gt.1.e-5_jpdblr) then
    krep=-101
    goto 1001
  elseif (2*ktronc+1.gt.knxlon) then
    krep=-84
    goto 1001
  elseif (2*ktronc+1.gt.4*(knlati/2)) then
!
!       le test ci-dessus est "dur" car il fait l'hypothese que,
!     dans le cas ou knlati est impair, la grille comporte les poles.
!
    krep=-79
    goto 1001
  endif
endif
!
if (kphase.eq.1) then
  goto 1001
endif
!**
!     2.  -  controle des tableaux (syntaxe et coherence).
!            (et de la pression de reference)
!-----------------------------------------------------------------------
!
200 continue
!
!
if (prefer.lt.0._jpdblr.or.                     &
&    prefer.gt.real (10*fa%mpresx, jpdblr)) then
  krep=-108
  goto 1001
endif
!
!     no mount everest test
!
if (.false.) then
do j=0,kniver
iprec=max (0_jplikb ,j-1)
zmin=min (pahybr(j),pbhybr(j))
zpmin=prefer*pahybr(j)+fa%spsmin*pbhybr(j)
zpmax=prefer*pahybr(j)+fa%spsmax*pbhybr(j)
zpminp=prefer*pahybr(iprec)+fa%spsmin*pbhybr(iprec)
zpmaxp=prefer*pahybr(iprec)+fa%spsmax*pbhybr(iprec)
!
if (zmin.lt.0._jpdblr.or.pbhybr(j).gt.1._jpdblr) then
  krep=-80
  goto 1001
elseif (j.ne.0.and.(pbhybr(j).lt.pbhybr(iprec).or.         &
&                 zpmin.le.zpminp.or.zpmax.le.zpmaxp)) then
  krep=-81
  goto 1001
endif
!
enddo
endif ! no mount everest test
!
if (.not.llmlam) then
!
   do j=1,inpahe
   iprec=max (1_jplikb ,j-1)
!
   if (knlopa(j).le.0.or.knlopa(j).gt.knxlon) then
     krep=-74
     goto 1001
   elseif (knlopa(j).lt.knlopa(iprec)) then
     krep=-75
     goto 1001
   elseif (knozpa(j).lt.0.or.knozpa(j).gt.ktronc) then
     krep=-76
     goto 1001
   elseif (knozpa(j).lt.knozpa(iprec)) then
     krep=-77
     goto 1001
   elseif ((2*knozpa(j)+1).gt.knlopa(j)) then
     krep=-78
     goto 1001
   elseif (abs (psinla(j)).gt.1._jpdblr) then
     krep=-102
     goto 1001
   elseif (psinla(j).ge.psinla(iprec).and.j.ne.1) then
     krep=-103
     goto 1001
   endif
!
   enddo
!
else
!
!        *****  error handling for lam case
!
   if (abs(knlopa(2)).gt.1) then
     krep=-117
     goto 1001
   elseif (knlopa(3).le.0.or.knlopa(3).gt.knxlon) then
     krep=-118
     goto 1001
   elseif (knlopa(4).lt.knlopa(3).or.knlopa(4).gt.knxlon) then
     krep=-119
     goto 1001
   elseif (knlopa(5).le.0.or.knlopa(5).gt.knlati) then
     krep=-120
     goto 1001
   elseif (knlopa(6).le.knlopa(5).or.knlopa(6).gt.knlati) then
     krep=-121
     goto 1001
   elseif (2*knlopa(7).gt.(knlopa(4)-knlopa(3))) then
     krep=-122
     goto 1001
   elseif (2*knlopa(8).gt.(knlopa(6)-knlopa(5))) then
     krep=-123
     goto 1001
   endif
!
endif
!
if (kphase.eq.2) goto 1001
!**
!     3.  -  controles lies a la definition du cadre proprement dite.
!-----------------------------------------------------------------------
!
300 continue
!
!        le nom de cadre specifie est-il deja defini ?
!
call fanuca_mt64                          &
&               (fa, cdnomc,krangc,.false.)
ldredf=krangc.ne.0
if (ldredf) goto 500
!
!        en arrivant ici, il s'agit donc d'un nouveau cadre.
!
if (fa%ncadef.ge.fa%jpnxca) then
!
!        trop de cadres deja definis pour en stocker un de plus.
!
  krep=-56
  goto 1001
endif
!
!       recherche d'un emplacement disponible dans les tables de cadres,
!     lequel devrait en bonne logique exister...
!
do j=1,fa%jpnxca
!
if (fa%cadre(j)%cnomca.eq.' ') then
  krangc=j
  goto 303
endif
!
enddo
!
krep=-66
goto 1001
!
303 continue
!
!           nouveau cadre, mise a jour des tables partagees de cadres.
!
fa%ncadef=fa%ncadef+1
fa%ncaind(fa%ncadef)=krangc

400 continue

call new_cadre (fa%cadre(krangc), ktyptr, knlati, ktronc, kniver)

fa%cadre(krangc)%cnomca=cdnomc
fa%cadre(krangc)%nlccad=klnomc
!**
!     4.  -  stockage des parametres du cadre (nouveau, ou redefini).
!-----------------------------------------------------------------------
!
fa%cadre(krangc)%nulcad=0
fa%cadre(krangc)%ntyptr=ktyptr
fa%cadre(krangc)%mtronc=ktronc
fa%cadre(krangc)%nniver=kniver
fa%cadre(krangc)%nlatit=knlati
fa%cadre(krangc)%nxlopa=knxlon
fa%cadre(krangc)%sslapo=pslapo
fa%cadre(krangc)%sclopo=pclopo
fa%cadre(krangc)%sslopo=pslopo
fa%cadre(krangc)%scodil=pcodil
fa%cadre(krangc)%sprefe=prefer
!
fa%cadre(krangc)%limlam=llmlam
fa%cadre(krangc)%nsflam=0
!
if (.not.ldredf.or.kgarde.ne.1) fa%cadre(krangc)%ngarde=kgarde
!
if (.not.llmlam) then
   icompt=0
!
   do j=1,inpahe
   icompt=icompt+knlopa(j)
   fa%cadre(krangc)%nlopar(j)=knlopa(j)
   fa%cadre(krangc)%nozpar(j)=knozpa(j)
   fa%cadre(krangc)%sinlat(j)=psinla(j)
   enddo
!
   if (knlati.eq.2*inpahe) then
     fa%cadre(krangc)%nvapdg=icompt*2
   else
     fa%cadre(krangc)%nvapdg=icompt*2-knlopa(inpahe)
   endif
!
else
! *****  calculation of knozpa(), then also setting of facom1-tables  *****
!
   imsmax = -ktyptr
   isflam = 0
   call ellips64  (ktronc,imsmax,ikntmp,ikmtmp)
!dp      call ellips(imsmax,ktronc,ikntmp,ikmtmp)
!
! initialisation de fa%nompar (du module famodu)
!
   fa%cadre(krangc)%nompar(2) = 0
   do jl=0,imsmax
     fa%cadre(krangc)%nompar(2*jl+3) = fa%cadre(krangc)%nompar(2*jl+2) + 1
     fa%cadre(krangc)%nompar(2*jl+4) = fa%cadre(krangc)%nompar(2*jl+3) &
&                             + 4*(ikntmp(jl)+1) -1
   enddo
   fa%cadre(krangc)%nompar(1) = ktronc
   fa%cadre(krangc)%nompar(2) = imsmax
!
   do jl=0,ktronc
      ik=ikmtmp(jl)
!dp         ik=ikntmp(jl)
      icpl4n(jl)=4*(ik+1)
      isflam = isflam + 4*(ik+1)
   enddo
!
   iesn0(0)=1
!
   do j=1,ktronc
      iesn0(j)=iesn0(j-1)+icpl4n(j-1)
   enddo
!
! -----  now setting of tables  -----
   do j=1,jnexpl
      fa%cadre(krangc)%nlopar(j)=knlopa(j)
   enddo
   do j=1,jngeom
      fa%cadre(krangc)%sinlat(j)=psinla(j)
   enddo
   fa%cadre(krangc)%nozpar(1)=ktronc
   fa%cadre(krangc)%nozpar(2)=imsmax
!
   do j=0,ktronc
      fa%cadre(krangc)%nozpar(2*j+3)=iesn0(j)
      fa%cadre(krangc)%nozpar(2*j+4)=iesn0(j)+icpl4n(j)-1
   enddo
  
   if (fa%cadre(krangc)%nozpar(2*ktronc+4).ne. &
&       fa%cadre(krangc)%nompar(2*imsmax+4))    &
&   then
     krep=-127
     goto 1001
   endif
!
   fa%cadre(krangc)%nsflam=isflam
!
! *****  determination of fa%nvapdg()  *****
!
   fa%cadre(krangc)%nvapdg=knlati*knxlon
!
endif
!
do j=0,kniver
fa%cadre(krangc)%sfohyb(1,j)=pahybr(j)
fa%cadre(krangc)%sfohyb(2,j)=pbhybr(j)
enddo
!
goto 1001
!**
!     5.  -  tentative de redefinition d'un cadre. controles ad hoc.
!-----------------------------------------------------------------------
!
500 continue
!
if (fa%cadre(krangc)%mtronc.ne.ktronc.or.fa%cadre(krangc)%nniver.ne.kniver.or. &
&    fa%cadre(krangc)%nlatit.ne.knlati.or.fa%cadre(krangc)%nxlopa.ne.knxlon.or. &
&    (abs(fa%cadre(krangc)%sslapo-pslapo)>zeps) .or.                      &
&    (abs(fa%cadre(krangc)%sclopo-pclopo)>zeps) .or.                      &
&    (abs(fa%cadre(krangc)%sslopo-pslopo)>zeps) .or.                      &
&    (abs(fa%cadre(krangc)%scodil-pcodil)>zeps) .or.                      &
&    fa%cadre(krangc)%ntyptr.ne.ktyptr.or.                                &
&    (abs(fa%cadre(krangc)%sprefe-prefer)>zeps)) goto 505
!
if (.not.llmlam) then
   do j=1,inpahe
   if (fa%cadre(krangc)%nlopar(j).ne.knlopa(j).or.                &
&       fa%cadre(krangc)%nozpar(j).ne.knozpa(j).or.                &
&       (abs(fa%cadre(krangc)%sinlat(j)-psinla(j))>zeps)) goto 505
   enddo
else
   do j=1,jnexpl
   if (fa%cadre(krangc)%nlopar(j).ne.knlopa(j)) goto 505
   enddo
   do j=1,jngeom
   if (abs(fa%cadre(krangc)%sinlat(j)-psinla(j))>zeps) goto 505
   enddo
endif
!
do j=0,kniver
if ((abs(fa%cadre(krangc)%sfohyb(1,j)-pahybr(j))>zeps).or.       &
&    (abs(fa%cadre(krangc)%sfohyb(2,j)-pbhybr(j))>zeps)) goto 505
enddo
!
!        si on arrive ici, il y a redefinition a l'identique,
!     du moins pour les parametres numeriques.
!        l'option de conservation du cadre peut, elle, etre modifiee
!     dans le cas d'une definition non dynamique.
!
if (kgarde.ne.1) fa%cadre(krangc)%ngarde=kgarde
goto 1001
!
505 continue
ldmodc=.true.
!
!        il y a donc redefinition avec changement de parametre(s),
!     ce qui n'est possible que s'il n'y a pas de fichier rattache,
!     et s'il ne s'agit pas d'une definition dynamique de cadre
!     (appel par faitou avec kgarde=1).
!
if (kgarde.eq.1) then
  krep=-58
elseif (fa%cadre(krangc)%nulcad.ne.0) then
  krep=-59
else
  call free_cadre (fa%cadre(krangc))
  goto 400
endif
!**
!    10.  -  phase terminale : messagerie eventuelle,
!            via le sous-programme "faipar" .
!-----------------------------------------------------------------------
!
1001 continue
!
llfata=krep.ne.0.and.fa%nrfaga.ne.2
!
if (fa%lfamop.or.llfata) then
  inimes=2
  clnspr='facadi'
  inumer=jpniil
!
  if (krep.eq.-65.and.ilcdno.le.0) then
    ilnomc=8
    clacti(1:ilnomc)=fa%chainc(:ilnomc)
  else
    ilnomc=min (klnomc,fa%ncpcad,int (len (clacti), jplikb))
    clacti(1:ilnomc)=cdnomc(1:ilnomc)
  endif
!
  write (unit=clmess,fmt='(''argum.simples='',i4,'','''''',a, &
&         '''''''',4('','',f7.4),4('','',i4),'','',f10.3,      &
&         2('','',l1),2('','',i2),'','',i3,'','',i1)')         &
&  krep,clacti(1:ilnomc),pslapo,pclopo,pslopo,pcodil,          &
&  ktronc,knlati,knxlon,kniver,prefer,ldmodc,ldredf,kphase,    &
&  krangc,klnomc,kgarde
  call faipar_mt64                                      &
&                 (fa, inumer,inimes,krep,.false.,clmess, &
&                  clnspr,clacti(1:ilnomc),.false.)
elseif (ktronc.le.fa%nstroi.and.(kphase.eq.0.or.kphase.eq.1)) then
  inimes=1
  clnspr='facadi'
  inumer=jpniil
  ilnomc=min (klnomc,fa%ncpcad)
  write (unit=clmess,                                              &
&         fmt='(''troncature ('',i2,'') inferieure '',              &
& ''ou egale a la sous-troncature "non compactee" implicite ('',i2, &
& ''), cadre '''''',a,'''''''')') ktronc,fa%nstroi,cdnomc(1:ilnomc)
  call faipar_mt64                                      &
&                 (fa, inumer,inimes,krep,.false.,clmess, &
&                  clnspr,clacti,.false.)
endif
!
if (lhook) call dr_hook('facadi_mt',1,zhook_handle)
end subroutine facadi_mt64



! oct-2012 p. marguinaud 64b lfi
subroutine facadi64                                       &
&           (krep, cdnomc, ktyptr, pslapo, pclopo, pslopo,  &
&           pcodil, ktronc, knlati, knxlon, knlopa, knozpa, &
&           psinla, kniver, prefer, pahybr, pbhybr, ldmodc, &
&           ldredf, kphase, krangc, klnomc, kgarde)
use fa_mod, only : fa => fa_com_default, &
&                   fa_com_default_init,  &
&                   new_fa_default
use lfi_precision
implicit none
! arguments
integer (kind=jplikb)  krep                                   ! in   
character (len=*)      cdnomc                                 ! in   
integer (kind=jplikb)  ktyptr                                 ! in   
real (kind=jpdblr)     pslapo                                 ! in   
real (kind=jpdblr)     pclopo                                 ! in   
real (kind=jpdblr)     pslopo                                 ! in   
real (kind=jpdblr)     pcodil                                 ! in   
integer (kind=jplikb)  ktronc                                 ! in   
integer (kind=jplikb)  knlati                                 ! in   
integer (kind=jplikb)  knxlon                                 ! in   
integer (kind=jplikb)  knlopa     (fa%jpxpah)                 ! in   
integer (kind=jplikb)  knozpa     (fa%jpxind)                 ! in   
real (kind=jpdblr)     psinla     (fa%jpxgeo)                 ! in   
integer (kind=jplikb)  kniver                                 ! in   
real (kind=jpdblr)     prefer                                 ! in   
real (kind=jpdblr)     pahybr     (0:kniver)                  ! in   
real (kind=jpdblr)     pbhybr     (0:kniver)                  ! in   
logical                ldmodc                                 ! in   
logical                ldredf                                 ! in   
integer (kind=jplikb)  kphase                                 ! in   
integer (kind=jplikb)  krangc                                 !   out
integer (kind=jplikb)  klnomc                                 ! inout
integer (kind=jplikb)  kgarde                                 ! in   

if (.not. fa_com_default_init) call new_fa_default ()

call facadi_mt64                                             &
&           (fa, krep, cdnomc, ktyptr, pslapo, pclopo, pslopo, &
&           pcodil, ktronc, knlati, knxlon, knlopa, knozpa,    &
&           psinla, kniver, prefer, pahybr, pbhybr, ldmodc,    &
&           ldredf, kphase, krangc, klnomc, kgarde)

end subroutine facadi64

subroutine facadi                                         &
&           (krep, cdnomc, ktyptr, pslapo, pclopo, pslopo,  &
&           pcodil, ktronc, knlati, knxlon, knlopa, knozpa, &
&           psinla, kniver, prefer, pahybr, pbhybr, ldmodc, &
&           ldredf, kphase, krangc, klnomc, kgarde)
use fa_mod, only : fa => fa_com_default, &
&                   fa_com_default_init,  &
&                   new_fa_default
use lfi_precision
implicit none
! arguments
integer (kind=jplikm)  krep                                   ! in   
character (len=*)      cdnomc                                 ! in   
integer (kind=jplikm)  ktyptr                                 ! in   
real (kind=jpdblr)     pslapo                                 ! in   
real (kind=jpdblr)     pclopo                                 ! in   
real (kind=jpdblr)     pslopo                                 ! in   
real (kind=jpdblr)     pcodil                                 ! in   
integer (kind=jplikm)  ktronc                                 ! in   
integer (kind=jplikm)  knlati                                 ! in   
integer (kind=jplikm)  knxlon                                 ! in   
integer (kind=jplikm)  knlopa     (fa%jpxpah)                 ! in   
integer (kind=jplikm)  knozpa     (fa%jpxind)                 ! in   
real (kind=jpdblr)     psinla     (fa%jpxgeo)                 ! in   
integer (kind=jplikm)  kniver                                 ! in   
real (kind=jpdblr)     prefer                                 ! in   
real (kind=jpdblr)     pahybr     (0:kniver)                  ! in   
real (kind=jpdblr)     pbhybr     (0:kniver)                  ! in   
logical                ldmodc                                 ! in   
logical                ldredf                                 ! in   
integer (kind=jplikm)  kphase                                 ! in   
integer (kind=jplikm)  krangc                                 !   out
integer (kind=jplikm)  klnomc                                 ! inout
integer (kind=jplikm)  kgarde                                 ! in   

if (.not. fa_com_default_init) call new_fa_default ()

call facadi_mt                                               &
&           (fa, krep, cdnomc, ktyptr, pslapo, pclopo, pslopo, &
&           pcodil, ktronc, knlati, knxlon, knlopa, knozpa,    &
&           psinla, kniver, prefer, pahybr, pbhybr, ldmodc,    &
&           ldredf, kphase, krangc, klnomc, kgarde)

end subroutine facadi

subroutine facadi_mt                                         &
&           (fa, krep, cdnomc, ktyptr, pslapo, pclopo, pslopo, &
&           pcodil, ktronc, knlati, knxlon, knlopa, knozpa,    &
&           psinla, kniver, prefer, pahybr, pbhybr, ldmodc,    &
&           ldredf, kphase, krangc, klnomc, kgarde)
use fa_mod, only : fa_com
use lfi_precision
implicit none
! arguments
type (fa_com)          fa                                     ! inout
integer (kind=jplikm)  krep                                   ! in   
character (len=*)      cdnomc                                 ! in   
integer (kind=jplikm)  ktyptr                                 ! in   
real (kind=jpdblr)     pslapo                                 ! in   
real (kind=jpdblr)     pclopo                                 ! in   
real (kind=jpdblr)     pslopo                                 ! in   
real (kind=jpdblr)     pcodil                                 ! in   
integer (kind=jplikm)  ktronc                                 ! in   
integer (kind=jplikm)  knlati                                 ! in   
integer (kind=jplikm)  knxlon                                 ! in   
integer (kind=jplikm)  knlopa     (fa%jpxpah)                 ! in   
integer (kind=jplikm)  knozpa     (fa%jpxind)                 ! in   
real (kind=jpdblr)     psinla     (fa%jpxgeo)                 ! in   
integer (kind=jplikm)  kniver                                 ! in   
real (kind=jpdblr)     prefer                                 ! in   
real (kind=jpdblr)     pahybr     (0:kniver)                  ! in   
real (kind=jpdblr)     pbhybr     (0:kniver)                  ! in   
logical                ldmodc                                 ! in   
logical                ldredf                                 ! in   
integer (kind=jplikm)  kphase                                 ! in   
integer (kind=jplikm)  krangc                                 !   out
integer (kind=jplikm)  klnomc                                 ! inout
integer (kind=jplikm)  kgarde                                 ! in   
! local integers
integer (kind=jplikb)  irep                                   ! in   
integer (kind=jplikb)  ityptr                                 ! in   
integer (kind=jplikb)  itronc                                 ! in   
integer (kind=jplikb)  inlati                                 ! in   
integer (kind=jplikb)  inxlon                                 ! in   
integer (kind=jplikb)  inlopa     (fa%jpxpah)                 ! in   
integer (kind=jplikb)  inozpa     (fa%jpxind)                 ! in   
integer (kind=jplikb)  iniver                                 ! in   
integer (kind=jplikb)  iphase                                 ! in   
integer (kind=jplikb)  irangc                                 !   out
integer (kind=jplikb)  ilnomc                                 ! inout
integer (kind=jplikb)  igarde                                 ! in   
! convert arguments

irep       = int (      krep, jplikb)
ityptr     = int (    ktyptr, jplikb)
itronc     = int (    ktronc, jplikb)
inlati     = int (    knlati, jplikb)
inxlon     = int (    knxlon, jplikb)
inlopa     = int (    knlopa, jplikb)
inozpa     = int (    knozpa, jplikb)
iniver     = int (    kniver, jplikb)
iphase     = int (    kphase, jplikb)
if (kphase==1) then
  ilnomc     = int (    klnomc, jplikb)
endif
igarde     = int (    kgarde, jplikb)

call facadi_mt64                                             &
&           (fa, irep, cdnomc, ityptr, pslapo, pclopo, pslopo, &
&           pcodil, itronc, inlati, inxlon, inlopa, inozpa,    &
&           psinla, iniver, prefer, pahybr, pbhybr, ldmodc,    &
&           ldredf, iphase, irangc, ilnomc, igarde)

krangc     = int (    irangc, jplikm)
if (kphase/=1) then
  klnomc     = int (    ilnomc, jplikm)
endif

end subroutine facadi_mt



!intf krep          in                                                                                                
!intf cdnomc        in                                                                                                
!intf ktyptr        in                                                                                                
!intf pslapo        in                                                                                                
!intf pclopo        in                                                                                                
!intf pslopo        in                                                                                                
!intf pcodil        in                                                                                                
!intf ktronc        in                                                                                                
!intf knlati        in                                                                                                
!intf knxlon        in                                                                                                
!intf knlopa        in    dims=fa%jpxpah                                                                              
!intf knozpa        in    dims=fa%jpxind                                                                              
!intf psinla        in    dims=fa%jpxgeo                                                                              
!intf kniver        in                                                                                                
!intf prefer        in                                                                                                
!intf pahybr        in    dims=0:kniver                                                                               
!intf pbhybr        in    dims=0:kniver                                                                               
!intf ldmodc        in                                                                                                
!intf ldredf        in                                                                                                
!intf kphase        in                                                                                                
!intf krangc          out                                                                                             
!intf klnomc        inout                                in_if=kphase==1                out_if=kphase/=1              
!intf kgarde        in                                                                                                
