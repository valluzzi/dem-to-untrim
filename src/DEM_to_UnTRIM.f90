MODULE tinclude2
   IMPLICIT none
   LOGICAL :: clone=.true., sparta=.false., RedBlack=.true.
   LOGICAL :: slim=.true., island=.false., flat=.false., argina=.true.
   INTEGER, PARAMETER :: dp = 8
   INTEGER, PARAMETER :: maxbc=999999, grd=1, nl=100, region=4  ! T, AI, AIT
   LOGICAL, ALLOCATABLE :: rb(:), liw(:,:)
   CHARACTER(len=80) :: DEM, grid_file, location
   INTEGER :: ibc(maxbc), jbc(maxbc), ne, nbc, imx, jmx, nsi, nsf, ihla, nv, ns, imax, jmax      &
             , cuntam, nlk, np, nr, bcE, BCW, bcN, bcS, imxg, jmxg, pixel1, xllcorner, yllcorner
   REAL :: dx(2), angle, hland, scale, delx, dely, latitude, apix
   REAL :: xmin, xmax, ymin, ymax
   REAL, ALLOCATABLE :: xy(:,:), xc(:,:), l12(:), h12(:), cq(:)
   INTEGER, ALLOCATABLE, DIMENSION (:) :: jk4, k4, k5, i4, i5, j4, j5, i7, kss, nns

   INTEGER, ALLOCATABLE, DIMENSION (:) :: ihw, ihu, jb, jt            , l4, l5, nls

   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: iw, iri, wik(:,:,:), nen, is, je
   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: ib, it, il, p, q, iwh, juh
   REAL, ALLOCATABLE :: sxy(:,:), xysb(:,:), xyst(:,:), aai(:), hwi(:), dyj(:), huj(:)

   INTEGER :: im, jm, iar=6000, ibr=5000          , tnl, nlkpicc=3872, nlkgra=139035

END MODULE tinclude2

PROGRAM DEM_to_UnTRIM
   USE tinclude2
   IMPLICIT none
   DEM = 'tests/lidar_V7.asc'
   CALL crono
   CALL datain
   CALL crono
   CALL setgrd
   CALL crono
   CALL riduci_grd
   CALL crono
END PROGRAM DEM_to_UnTRIM


SUBROUTINE datain
   USE tinclude2
   IMPLICIT none
   INTEGER :: i, j, k, l, n, m, nm, ne0, ihsea

!**** Chiste � na' bellezza! *********************

   OPEN(grd,file=DEM,status='old',action='read')

      READ(grd,*)location,imx
      READ(grd,*)location,jmx
      READ(grd,*)location,xllcorner
      READ(grd,*)location,yllcorner
      READ(grd,*)location,delx
      dely=delx
      READ(grd,*)location,ihla

   imax = CEILING(real(imx)/nl)
   jmax = CEILING(real(jmx)/nl)
   scale=0.01
!   hland=scale*ihla
   hland=ihla
   latitude = 0.0

   BCE = 1
   BCW = 0
   BCN = 1
   BCS = 0

   ALLOCATE (iw(imax*nl,jmax*nl),hwi(imx))

   iw = -290000
   iw = ihla/scale
   DO j=jmx,1,-1
      READ(grd,*)hwi
      iw(1:imx,j) = NINT(hwi/scale)
   END DO
   DEALLOCATE(hwi)
   CLOSE(grd)

print *,'xxxxx',ihla, MINVAL(iw,scale*iw/=ihla),' < iw < ',MAXVAL(iw,scale*iw/=ihla)

   IF(argina)THEN
      CALL assottiglia
!      WHERE(iw>ibr)iw=-290000  ! arginiamo
   ELSE
      WHERE(iw>ibr)iw=iw-iar  ! non arginiamo
   END IF

   IF(region==1)location = 'Regione Campania'
   IF(region==2)location = 'Puglia e Basilicata'
   IF(region==3)location = 'Puglia, Basilicata e Campania'
   IF(region==4)location = 'Cesenatico'

   WRITE(*,*)TRIM(location),' -- clone, slim, flat, argina, RedBlack=',clone,slim,flat,argina,RedBlack

!IF(sparta)WHERE(iw>59999)iw=-290000-10             !   controlla spartiacque
IF(sparta)WHERE(iw>59999)iw=-290000                !   spartiacque = quota + 60000
IF(.not.sparta)WHERE(iw>59999)iw=iw-60000          !   ignora spartiacque

iw =-iw

WHERE(iw==290000)iw=-290000       !   cura spartiacque

!ihsea = MERGE(-290000-10,-290000,island)
ihsea = -290000-10

IF(island)THEN
   ALLOCATE(liw(imax*nl,jmax*nl))
   liw = .false.
   liw( 1 , : ) = iw( 1 , : )*scale+ihla==0
   liw(imx, : ) = iw(imx, : )*scale+ihla==0
   liw( : , 1 ) = iw( : , 1 )*scale+ihla==0
   liw( : ,jmx) = iw( : ,jmx)*scale+ihla==0
   nm=1
   DO WHILE (nm>0)
      nm = 0
      DO i=2,imx-1
         DO j=2,jmx-1
            IF(liw(i,j).or.iw(i,j+1)*scale+ihla/=0)CYCLE
            liw(i,j) = liw(i-1,j).or.liw(i+1,j).or.liw(i,j-1).or.liw(i,j+1)
            IF(liw(i,j))nm=nm+1
         END DO
         DO j=jmx-1,2,-1
            IF(liw(i,j).or.iw(i,j-1)*scale+ihla/=0)CYCLE
            liw(i,j) = liw(i-1,j).or.liw(i+1,j).or.liw(i,j-1).or.liw(i,j+1)
            IF(liw(i,j))nm=nm+1
         END DO
      END DO
      DO j=2,jmx-1
         DO i=2,imx-1
            IF(liw(i,j).or.iw(i+1,j)*scale+ihla/=0)CYCLE
            liw(i,j) = liw(i-1,j).or.liw(i+1,j).or.liw(i,j-1).or.liw(i,j+1)
            IF(liw(i,j))nm=nm+1
         END DO
         DO i=imx-1,2,-1
            IF(liw(i,j).or.iw(i-1,j)*scale+ihla/=0)CYCLE
            liw(i,j) = liw(i-1,j).or.liw(i+1,j).or.liw(i,j-1).or.liw(i,j+1)
            IF(liw(i,j))nm=nm+1
         END DO
      END DO

      print *,nm,count(liw)
   END DO
   WHERE(liw)iw = ihsea
   DEALLOCATE(liw)
ELSE
   n=0.5092*imx              !   Openbc
   m=0.39525*jmx
   DO j=jmx,m,-1
      DO i=imx,n,-1
         IF(iw(i,j)+ihla<0)EXIT
         iw(i,j) = ihsea
      END DO
   END DO
   DO i=imx,n,-1
      DO j=jmx,m,-1
         IF(iw(i,j)/=ihsea.and.iw(i,j)+ihla<0)EXIT
         iw(i,j) = ihsea
      END DO
   END DO
END IF

ihla = -290000
hland=ihla*scale
WHERE(iw==999900)iw = ihla

print *,'ooooo',MINVAL(iw), MINVAL(iw,iw>ihla),' < iw < ',MAXVAL(iw)

   im = 0.5*imx
   jm = 0.5*jmx

   CALL crono

!   OPEN(grd,file='DEM2.grd')
!   WRITE(grd,NML=listgrd)
!   DO j=jmx,1,-1
!      WRITE(grd,'(20i5)')iw(1:imx,j)
!   END DO
!   CLOSE(grd)

   dx    = nl*(/delx,dely/)
   np    = nl**2
   apix  = delx*dely
   angle = latitude

   ALLOCATE(p(nl,nl), q(nl,nl))

   imxg = imax
   jmxg = jmax
   pixel1 = count(iw>ihla)

   IF(slim)CALL stoscet

   ne = 0
   DO i=1,imxg
	  DO j=jmxg,1,-1
         p = iw(nl*(i-1)+1:nl*i,nl*(j-1)+1:nl*j)
         WHERE(p<-ibr)p=ihla
         DO WHILE(MAXVAL(p)>ihla)
			ne=ne+1
            CALL sconta
         END DO
	  END DO
   END DO
   WRITE(*,'(a28,3i10)')'  DEM size: imx , jmx , nl =',imx,jmx,nl
   WRITE(*,'(a28,3i10)')' Grid size: imxg, jmxg     =',imxg,jmxg 
   WRITE(*,'(a28,4i10)')' pixels0,  pixels1,  pixels2,  iter  =',imx*jmx,pixel1,count(iw>ihla) !,cuntam

CONTAINS

SUBROUTINE assottiglia
   INTEGER :: na, nb
   na = count(iw>ibr)
   nb = na+1
   DO WHILE(na<nb)
      nb = na

      DO i=2,imx-1
         DO j=2,jmx-1
            IF(iw(i,j)<ibr.or.iw(i-1,j-1)>ibr)CYCLE
            IF(iw(i-1,j)<ibr.and.iw(i,j-1)<ibr.and.iw(i+1,j)>ibr.and.iw(i,j+1)>ibr)iw(i,j)=iw(i,j)-iar
         END DO
         DO j=jmx-1,2,-1
            IF(iw(i,j)<ibr.or.iw(i-1,j+1)>ibr)CYCLE
            IF(iw(i-1,j)<ibr.and.iw(i,j-1)>ibr.and.iw(i+1,j)>ibr.and.iw(i,j+1)<ibr)iw(i,j)=iw(i,j)-iar
         END DO
      END DO

      DO i=imx-1,2,-1
         DO j=2,jmx-1
            IF(iw(i,j)<ibr.or.iw(i+1,j-1)>ibr)CYCLE
            IF(iw(i-1,j)>ibr.and.iw(i,j-1)<ibr.and.iw(i+1,j)<ibr.and.iw(i,j+1)>ibr)iw(i,j)=iw(i,j)-iar
         END DO
         DO j=jmx-1,2,-1
            IF(iw(i,j)<ibr.or.iw(i+1,j+1)>ibr)CYCLE
            IF(iw(i-1,j)>ibr.and.iw(i,j-1)>ibr.and.iw(i+1,j)<ibr.and.iw(i,j+1)<ibr)iw(i,j)=iw(i,j)-iar
         END DO
      END DO

      DO i=2,imx-1
         DO j=2,jmx-1
            IF(iw(i,j)<ibr.or.iw(i+1,j-1)>ibr.or.iw(i-1,j-1)>ibr)CYCLE
            IF(iw(i+1,j)<ibr.and.iw(i-1,j)<ibr.and.iw(i,j+1)>ibr.and.iw(i,j-1)<ibr)   &
                                                                iw(i,j)=iw(i,j)-iar
         END DO
         DO j=jmx-1,2,-1
            IF(iw(i,j)<ibr.or.iw(i+1,j+1)>ibr.or.iw(i-1,j+1)>ibr)CYCLE
            IF(iw(i+1,j)<ibr.and.iw(i-1,j)<ibr.and.iw(i,j+1)<ibr.and.iw(i,j-1)>ibr)   &
                                                                iw(i,j)=iw(i,j)-iar
         END DO
      END DO 

      DO j=2,jmx-1
         DO i=2,imx-1
            IF(iw(i,j)<ibr.or.iw(i-1,j+1)>ibr.or.iw(i-1,j-1)>ibr)CYCLE
            IF(iw(i+1,j)>ibr.and.iw(i-1,j)<ibr.and.iw(i,j+1)<ibr.and.iw(i,j-1)<ibr)   &
                                                                iw(i,j)=iw(i,j)-iar
         END DO
         DO i=imx-1,2,-1
            IF(iw(i,j)<ibr.or.iw(i+1,j+1)>ibr.or.iw(i+1,j-1)>ibr)CYCLE
            IF(iw(i+1,j)<ibr.and.iw(i-1,j)>ibr.and.iw(i,j+1)<ibr.and.iw(i,j-1)<ibr)   &
                                                                iw(i,j)=iw(i,j)-iar
         END DO
      END DO
      na = count(iw>ibr)

!      print *,'pipe',nb,na
   END DO

END SUBROUTINE assottiglia

END SUBROUTINE datain


SUBROUTINE stoscet
   USE tinclude2
   IMPLICIT none
   INTEGER :: i, j, n, m, ip=0, iq
   ALLOCATE(liw(imax*nl,jmax*nl))
   cuntam = 0

   liw = .false.

   IF(region==1)THEN
      j = 0.5*jmx       !   Tirreno
      n = 0.2*imx
      liw(1:n,1:j) = iw(1:n,1:j)>=-6000
   ELSE IF(region==2)THEN
      j = 0.5*imx       !   Adriatico-Ionio
      liw(j:imx,:) = iw(j:imx,:)>=-6000
   ELSE IF(region==3)THEN
      j = 0.5*imx       !   Adriatico-Ionio
      liw(j:imx,:) = iw(j:imx,:)>=-6000
      j = 0.5*jmx       !   Tirreno
      n = 0.2*imx
      liw(1:n,1:j) = iw(1:n,1:j)>=-6000
   ELSE IF(region==4)THEN
      i = 0.5*imx       !   Cesenatico
      j = 0.24*jmx
      liw(i:imx,jmx-j:jmx) = iw(i:imx,jmx-j:jmx)>=0
   ELSE
      RETURN
   END IF

   iq = COUNT(liw)
   DO WHILE(ip<iq)
      cuntam = cuntam+1
      ip = iq
      DO n=2,imx-1
         DO m=2,jmx-1
            IF(liw(n,m).or.iw(n,m)<=ihla)CYCLE
            liw(n,m) = liw(n-1,m).or.liw(n+1,m).or.liw(n,m-1).or.liw(n,m+1)
         END DO
      END DO

      DO n=imx-1,2,-1
         DO m=jmx-1,2,-1
            IF(liw(n,m).or.iw(n,m)<=ihla)CYCLE
            liw(n,m) = liw(n-1,m).or.liw(n+1,m).or.liw(n,m-1).or.liw(n,m+1)
         END DO
      END DO

      n=1
      DO m=2,jmx-1
         IF(liw(n,m).or.iw(n,m)<=ihla)CYCLE
         liw(n,m) = liw(n+1,m).or.liw(n,m-1).or.liw(n,m+1)
      END DO

      n=imx
      DO m=2,jmx-1
         IF(liw(n,m).or.iw(n,m)<=ihla)CYCLE
         liw(n,m) = liw(n-1,m).or.liw(n,m-1).or.liw(n,m+1)
      END DO

      m=1
      DO n=2,imx-1
         IF(liw(n,m).or.iw(n,m)<=ihla)CYCLE
         liw(n,m) = liw(n-1,m).or.liw(n+1,m).or.liw(n,m+1)
      END DO

      m=jmx
      DO n=2,imx-1
         IF(liw(n,m).or.iw(n,m)<=ihla)CYCLE
         liw(n,m) = liw(n-1,m).or.liw(n+1,m).or.liw(n,m-1)
      END DO

      n=1
      m=1
      IF(.not.liw(n,m).and.iw(n,m)>ihla)liw(n,m) = liw(n+1,m).or.liw(n,m+1)
      n=imx
      m=1
      IF(.not.liw(n,m).and.iw(n,m)>ihla)liw(n,m) = liw(n-1,m).or.liw(n,m+1)
      n=1
      m=jmx
      IF(.not.liw(n,m).and.iw(n,m)>ihla)liw(n,m) = liw(n+1,m).or.liw(n,m-1)
      n=imx
      m=jmx
      IF(.not.liw(n,m).and.iw(n,m)>ihla)liw(n,m) = liw(n-1,m).or.liw(n,m-1)

      iq = COUNT(liw)
   END DO
   liw = .not.liw.and.iw>ihla
   WHERE(liw)iw = ihla
   DEALLOCATE(liw)
END SUBROUTINE stoscet


SUBROUTINE setgrd
   USE tinclude2
   IMPLICIT none
   INTEGER :: i, j, n, m, k, ki, kj, kk, nlk0, cck, uuhh(nl), ENWS(4), nn
   REAL :: xl, xr, yb, yt
   LOGICAL :: ok
   ALLOCATE (nen(4,ne),wik(ne,nl,nl),rb(ne),xy(2,4*ne),ib(imxg,jmxg),it(imxg,jmxg),iwh(np,ne))
!   ALLOCATE (il(2,100*ne),l12(100*ne),h12(100*ne),iwh(np,ne))
   ALLOCATE (l4(nlkpicc),l5(nlkpicc),nls(nlkpicc),il(2,nlkpicc),l12(nlkgra),h12(nlkgra),cq(nlkgra))
   l12 = 0.0
   h12 = 0.0
   cq  = 0.41
   wik = ihla
   nls = 0
   l4 = 0
   l5 = 0
   il = 0
   ib = 0
   it = 0
   ne = 0
   nv = 0
   nbc= 0
   nen= 0
   nlk= 0
   iwh = ihla
   xy = 0.0
   DO i=1,imxg
	  xl=(i-1)*dx(1)
	  xr=i*dx(1)
	  DO j=jmxg,1,-1
		 yb=(j-1)*dx(2)
		 yt=j*dx(2)
         p = iw (nl*(i-1)+1:nl*i,nl*(j-1)+1:nl*j)
         WHERE(p<-ibr)p=ihla

         cuntam = 0
         DO WHILE(MAXVAL(p)>ihla)
			ne=ne+1
            CALL sconta
            IF(i==imxg.and.bcE==1.and.MAXVAL(q(nl,:))>ihla)CALL add(ne)
            IF(i== 1  .and.bcW==1.and.MAXVAL(q(1 ,:))>ihla)CALL add(ne)
            IF(j==jmxg.and.bcN==1.and.MAXVAL(q(:,nl))>ihla)CALL add(ne)
            IF(j== 1  .and.bcS==1.and.MAXVAL(q(:,1 ))>ihla)CALL add(ne)

            IF(i<imxg)THEN
               IF(ANY(q>ihla.and.iw(nl*(i-1)+2:nl*i+1,nl*(j-1)+1:nl*j)<ihla))CALL add(ne)
            END IF
            IF(i> 1  )THEN
               IF(ANY(q>ihla.and.iw(nl*(i-1)  :nl*i-1,nl*(j-1)+1:nl*j)<ihla))CALL add(ne)
            END IF
            IF(j<jmxg)THEN
               IF(ANY(q>ihla.and.iw(nl*(i-1)+1:nl*i,nl*(j-1)+2:nl*j+1)<ihla))CALL add(ne)
            END IF
            IF(j> 1  )THEN
               IF(ANY(q>ihla.and.iw(nl*(i-1)+1:nl*i,nl*(j-1)  :nl*j-1)<ihla))CALL add(ne)
            END IF

            wik (ne,:,:) = q
            rb(ne) = 2*INT(0.5*(i+j)) == i+j
            iwh (:,ne) = RESHAPE(q ,(/np/))
            cuntam = cuntam+1
            IF(cuntam==1)ib(i,j)=ne
            it(i,j)=ne
            IF(cuntam>1)THEN
               nen(:,ne)=nen(:,ne-1)
            ELSE
			   IF(nen(1,ne)==0)THEN
			      nv=nv+1
                  xy(:,nv) = [xr,yb]
			      nen(1,ne)=nv
			   END IF
			   DO k=nv-1,1,-1
			      IF(xy(1,k)==xr.and.xy(2,k)==yt)nen(2,ne)=k
			      IF(xy(1,k)==xl.and.xy(2,k)==yt)nen(3,ne)=k
			      IF(xy(1,k)==xl.and.xy(2,k)==yb)nen(4,ne)=k
			      IF(ALL(nen(:,ne)>0))EXIT
			   END DO
			   IF(nen(2,ne)==0)THEN
			      nv=nv+1
                  xy(:,nv) = [xr,yt]
			      nen(2,ne)=nv
			   END IF
			   IF(nen(3,ne)==0)THEN
			      nv=nv+1
                  xy(:,nv) = [xl,yt]
			      nen(3,ne)=nv
			   END IF
			   IF(nen(4,ne)==0)THEN
			      nv=nv+1
                  xy(:,nv) = [xl,yb]
			      nen(4,ne)=nv
			   END IF
            END IF
         END DO

! Inserire qui i links fra i vari cloni quando essi sono separati ta pixels<-ibr e >ihla
         nlk0 = nlk+1
         p = iw (nl*(i-1)+1:nl*i,nl*(j-1)+1:nl*j)
         WHERE(p>-ibr)p = ihla
         DO n=1,nl
            DO m=1,nl
               IF(p(n,m)<=ihla)CYCLE

               ENWS = 0
               DO ki=ib(i,j),it(i,j)
                  IF(n<nl)THEN
                     IF(wik(ki,n+1,m)>ihla)ENWS(1) = ki
                  END IF
                  IF(m<nl)THEN
                     IF(wik(ki,n,m+1)>ihla)ENWS(2) = ki
                  END IF
                  IF(n>1)THEN
                     IF(wik(ki,n-1,m)>ihla)ENWS(3) = ki
                  END IF
                  IF(m>1)THEN
                     IF(wik(ki,n,m-1)>ihla)ENWS(4) = ki
                  END IF
               END DO
               IF(COUNT(ENWS>0)<2)CYCLE
               ok = .false.
               DO ki=1,3
                  DO kj=ki+1,4
                     IF(ENWS(ki)==ENWS(kj).or.ENWS(ki)*ENWS(kj)<=0)CYCLE
                     DO kk = nlk0,nlk
                        IF((il(1,kk)==ENWS(ki).and.il(2,kk)==ENWS(kj).or.    &
                            il(1,kk)==ENWS(kj).and.il(2,kk)==ENWS(ki)))THEN
                           DO nn=l4(kk),l5(kk)
                              IF(ABS(h12(nn)-(p(n,m)+iar)*scale)<0.01)THEN
                                 l12(nn) = l12(nn)+delx
                                 ok = .true.
                                 EXIT
                              END IF
                           END DO
                           IF(ok)EXIT
                           nls(kk)      = nls(kk)+1
                           l5(kk) = l5(kk)+1
                           h12(l5(kk)) = (p(n,m)+iar)*scale
                           l12(l5(kk)) = delx
                           ok = .true.
                           EXIT
                        END IF
                     END DO
                     IF(ok)EXIT
                     nlk       = nlk+1               !   nuovo link: ENWS(ki) e ENWS(kj)
                     IF(nlk==1)THEN
                        l4(nlk) = 1
                     ELSE
                        l4(nlk) = l5(nlk-1)+1
                     END IF
                     l5(nlk)   = l4(nlk)
                     nls(nlk)  = 1
                     il(:,nlk) = [ENWS(ki),ENWS(kj)]
                     h12(l4(nlk))  = (p(n,m)+iar)*scale
                     l12(l4(nlk))  = delx
                     IF(.not.rb(ENWS(kj)))rb(ENWS(ki)) = .true.
                     ok = .true.
                     EXIT
                  END DO
                  IF(ok)EXIT
               END DO

            END DO
         END DO


	  END DO
   END DO
   DEALLOCATE(iw)

print *,'COUNT(l12(1:l5(nlk))<=0.0) =',COUNT(l12(1:l5(nlk))<=0.0),MINVAL(l12(1:l5(nlk)))

   IF(flat)THEN
      DO ki=1,ne
         q = wik(ki,:,:)
         CALL flatta3(q)
         wik (ki,:,:) = q
         iwh (:,ki) = RESHAPE(q ,(/np/))
      END DO
   END IF

   ALLOCATE (juh(nl,2*ne),is(4,ne),jb(4*ne),jt(4*ne),je(2,4*ne))

   CALL setns

   DO i=1,ne                 !        reorder bc polygons
      DO j=1,nbc
         IF(i==ibc(j))THEN
	        CALL exchange_polygons(j,i)
	        EXIT
	     END IF
      END DO
   END DO

print *,'nv, nbc, nlk =', nv, nbc, nlk

nr = ne
IF(.not.RedBlack)return

   rb(1:nbc) = .true.                 !        reorder polygons
   nr = nbc
   k  = ne
   DO i=nbc+1,ne
      IF(i>k)EXIT
      IF(rb(i))THEN
         nr=nr+1
      ELSE
         DO j=k,i+1,-1
            IF(rb(j))THEN
               nr=nr+1
               CALL exchange_polygons(i,j)
               k=j-1
               EXIT
            END IF
         END DO
      END IF
   END DO

print *, 'rossi & neri, r+n =',nr,ne-nr,ne
print *,'Kontrolliamo:',COUNT(rb),count(rb(1:nr)),rb(nr+1)

   DEALLOCATE(rb,wik)

CONTAINS 

SUBROUTINE flatta3(q)
   INTEGER  :: lxn, lyn, q(nl,nl)
   REAL(dp) :: land, x, y, xc, yc, E0, A(3,3), b(3), Sx, Sy, detA, volu
   xc = 0.5_dp*REAL(nl+1)
   yc = 0.5_dp*REAL(nl+1)
   A  = 0.0_dp
   b  = 0.0_dp
   land = REAL(COUNT(q>ihla))
   volu = REAL(SUM(q,q>ihla))
   Sx = 0.0_dp
   Sy = 0.0_dp

   DO i=1,nl
      x = REAL(i)-xc
      DO j=1,nl
         IF(q(i,j)<=ihla)CYCLE
         y = REAL(j)-yc
         A(1,1) = A(1,1)+1.0_dp
         A(1,2) = A(1,2)+x
         A(1,3) = A(1,3)+y
         A(2,2) = A(2,2)+x*x
         A(2,3) = A(2,3)+x*y
         A(3,3) = A(3,3)+y*y
         b(1)   = b(1)-REAL(q(i,j))
         b(2)   = b(2)-REAL(q(i,j))*x
         b(3)   = b(3)-REAL(q(i,j))*y
      END DO
   END DO

   A(2,1) = A(1,2)
   A(3,1) = A(1,3)
   A(3,2) = A(2,3)

   detA = A(1,1)*A(2,2)*A(3,3)+A(1,2)*A(2,3)*A(3,1)+A(1,3)*A(2,1)*A(3,2)   &
         -A(3,1)*A(2,2)*A(1,3)-A(3,2)*A(2,3)*A(1,1)-A(3,3)*A(2,1)*A(1,2)
   IF(ABS(detA)>0.001)THEN
      E0 = (b(1)*A(2,2)*A(3,3)+A(1,2)*A(2,3)*b(3)+A(1,3)*b(2)*A(3,2)     &
           -b(3)*A(2,2)*A(1,3)-A(3,2)*A(2,3)*b(1)-A(3,3)*b(2)*A(1,2))/detA
      Sx = (A(1,1)*b(2)*A(3,3)+b(1)*A(2,3)*A(3,1)+A(1,3)*A(2,1)*b(3)     &
           -A(3,1)*b(2)*A(1,3)-b(3)*A(2,3)*A(1,1)-A(3,3)*A(2,1)*b(1))/detA
      Sy = (A(1,1)*A(2,2)*b(3)+A(1,2)*b(2)*A(3,1)+b(1)*A(2,1)*A(3,2)     &
        -A(3,1)*A(2,2)*b(1)-A(3,2)*b(2)*A(1,1)-b(3)*A(2,1)*A(1,2))/detA
   ELSE
      E0 = -volu/land
!      print *,REAL(sx),REAL(sy),REAL(detA),count(q>ihla)
   END IF

   DO i=1,nl
      x = REAL(i)-xc
      DO j=1,nl
         y = REAL(j)-yc
!         IF(q(i,j)>ihla)q(i,j) = q(i,j)+INT(E0+x*Sx+y*Sy)
         IF(q(i,j)>ihla)q(i,j) = q(i,j)+INT(x*Sx+y*Sy)
       END DO
   END DO

END SUBROUTINE flatta3

SUBROUTINE add(i)
   USE tinclude2
   IMPLICIT none
   INTEGER :: i, j, n
   IF(ANY(ibc(1:nbc)==i))RETURN
   nbc=nbc+1
   IF(nbc.gt.maxbc)STOP 'Not enough maxbc.'
   ibc(nbc)=i
END SUBROUTINE add

SUBROUTINE setns
      INTEGER :: l, n, m
	  is=0
	  ns=0
	  jb=0
	  jt=0
	  je=0
      juh  = ihla
      DO i=1,imxg-1   !  lati verticali
         DO j=1,jmxg
            IF(ib(i,j)<=0.or.ib(i+1,j)<=0)CYCLE
            DO n=ib(i,j),it(i,j)
               DO m=ib(i+1,j),it(i+1,j)
                  uuhh = MIN(wik(n,nl,:),wik(m,1,:))
                  cck  = MAXVAL(uuhh)
                  IF(cck<=ihla)CYCLE
                  ns = ns+1
                  is(1,n) = ns
                  is(3,m) = ns
                  jb(ns) = nen(1,n)
                  jt(ns) = nen(2,n)
                  je(:,ns) = [n,m]
                  juh (:,ns) = uuhh
               END DO
            END DO
         END DO
      END DO

      DO i=1,imxg   !  lati orizzontali
         DO j=1,jmxg-1
            IF(ib(i,j)<=0.or.ib(i,j+1)<=0)CYCLE
            DO n=ib(i,j),it(i,j)
               DO m=ib(i,j+1),it(i,j+1)
                  uuhh = MIN(wik(n,:,nl),wik(m,:,1))
                  cck  = MAXVAL(uuhh)
                  IF(cck<=ihla)CYCLE
                  ns = ns+1
                  is(2,n)=ns
                  is(4,m)=ns
                  jb(ns) = nen(3,n)
                  jt(ns) = nen(2,n)
                  je(:,ns) = [m,n]
                  juh (:,ns) = uuhh
               END DO
            END DO
         END DO
      END DO

nsi = ns
print *,'ne0, ne, SUM(it-ib)',ne-SUM(it-ib),ne,SUM(it-ib)
DEALLOCATE(ib,it)

      DO i=1,ne   !  contorno
         IF(is(1,i)==0)THEN
            ns = ns+1
            is(1,i)=ns
            jb(ns) = nen(1,i)
            jt(ns) = nen(2,i)
            je(1,ns) = i
         END IF
         IF(is(2,i)==0)THEN
            ns = ns+1
            is(2,i)=ns
            jb(ns) = nen(3,i)
            jt(ns) = nen(2,i)
            je(1,ns) = i
         END IF
         IF(is(3,i)==0)THEN
            ns = ns+1
            is(3,i)=ns
            jb(ns) = nen(4,i)
            jt(ns) = nen(3,i)
            je(1,ns) = i
         END IF
         IF(is(4,i)==0)THEN
            ns = ns+1
            is(4,i)=ns
            jb(ns) = nen(4,i)
            jt(ns) = nen(1,i)
            je(1,ns) = i
         END IF
      END DO
      nsf = nsi    !    inflow edges
      xmin = MINVAL(xy(1,1:nv))
      xmax = MAXVAL(xy(1,1:nv))
      ymin = MINVAL(xy(2,1:nv))
      ymax = MAXVAL(xy(2,1:nv))

      IF(bcE==2)THEN
         DO j=nsi+1,ns
            IF(xy(1,jb(j))<xmax.or.xy(1,jt(j))<xmax)CYCLE
            uuhh = wik(je(1,j),nl,:)
            cck  = MAXVAL(uuhh)
            IF(cck<=ihla)CYCLE
            nsf=nsf+1
            juh (:,nsf) = uuhh
            CALL exchange_edges(nsf,j)
         END DO
      END IF
      IF(bcW==2)THEN
         DO j=nsi+1,ns
            IF(xy(1,jb(j))>xmin.or.xy(1,jt(j))>xmin)CYCLE
            uuhh = wik(je(1,j),1,:)
            cck  = MAXVAL(uuhh)
            IF(cck<=ihla)CYCLE
            nsf=nsf+1
            juh (:,nsf) = uuhh
            CALL exchange_edges(nsf,j)
         END DO
      END IF

      IF(bcN==2)THEN
         DO j=nsi+1,ns
            IF(xy(2,jb(j))<ymax.or.xy(2,jt(j))<ymax)CYCLE
            uuhh = wik(je(1,j),:,nl)
            cck  = MAXVAL(uuhh)
            IF(cck<=ihla)CYCLE
            nsf=nsf+1
            juh (:,nsf) = uuhh
            CALL exchange_edges(nsf,j)
         END DO
      END IF
      IF(bcS==2)THEN
         DO j=nsi+1,ns
            IF(xy(2,jb(j))>ymin.or.xy(2,jt(j))>ymin)CYCLE
            uuhh = wik(je(1,j),:,1)
            cck  = MAXVAL(uuhh)
            IF(cck<=ihla)CYCLE
            nsf=nsf+1
            juh (:,nsf) = uuhh
            CALL exchange_edges(nsf,j)
         END DO
      END IF

   print *,'nsi, nsf, ns tutti =', nsi, nsf, ns

END SUBROUTINE setns

END SUBROUTINE setgrd


SUBROUTINE exchange_edges(i,j)
   USE tinclude2                                    !    Global variables
   IMPLICIT none
   INTEGER :: i, j, k
   k       = je(1,i)
   je(1,i) = je(1,j)
   je(1,j) = k
   k     = jb(i)
   jb(i) = jb(j)
   jb(j) = k
   k     = jt(i)
   jt(i) = jt(j)
   jt(j) = k
   WHERE(is== i)is = -j
   WHERE(is== j)is =  i
   WHERE(is==-j)is =  j
END SUBROUTINE exchange_edges


SUBROUTINE exchange_polygons(i,k)
   USE tinclude2                                    !    Global variables
   IMPLICIT none
   INTEGER :: i, j, k, ktemp, atemp(np) !, mpo(nl,nl)
   LOGICAL :: ltemp
   DO j=1,4
	  ktemp    = nen(j,i)
	  nen(j,i) = nen(j,k)
	  nen(j,k) = ktemp
	  ktemp    = is(j,i)
	  is(j,i)  = is(j,k)
	  is(j,k)  = ktemp
	END DO
    WHERE(je== i)je = -k
    WHERE(je== k)je = i
    WHERE(je==-k)je = k
    ltemp = rb(i)
    rb(i) = rb(k)
    rb(k) = ltemp
	atemp    = iwh(:,i)
	iwh(:,i) = iwh(:,k)
	iwh(:,k) = atemp

    WHERE(il== i)il = -k
    WHERE(il== k)il = i
    WHERE(il==-k)il = k
    if (i == 1) then
      print *, "exchange_polygons:",i , "with ", k
    end if 

END SUBROUTINE exchange_polygons


SUBROUTINE sconta
   USE tinclude2                                    !    Global variables
   IMPLICIT none
   INTEGER :: i, j, n, m, k
   LOGICAL :: ok, mask(nl,nl), flag(nl,nl)
   mask = p>ihla
   flag = .false.
   ok   = .false.
   k    = 0
   DO i=1,nl
      DO j=1,nl
         IF(.not.mask(i,j))CYCLE
         flag(i,j) = .true.
         DO WHILE(k<COUNT(flag))
            k = COUNT(flag)
            DO n=1,nl
               DO m=1,nl
                  IF(.not.mask(n,m))CYCLE
                  IF(n>1)THEN
                     IF(flag(n-1,m))flag(n,m)=.true.
                  END IF
                  IF(n<nl)THEN
                     IF(flag(n+1,m))flag(n,m)=.true.
                  END IF
                  IF(m>1)THEN
                     IF(flag(n,m-1))flag(n,m)=.true.
                  END IF
                  IF(m<nl)THEN
                     IF(flag(n,m+1))flag(n,m)=.true.
                  END IF
               END DO
            END DO
         END DO
         ok = .true.
         EXIT
      END DO
      IF(ok)EXIT
   END DO
   q  = MERGE(p ,ihla,flag)
   IF(.not.clone)q =p                             ! non sconta
   WHERE(p<ihla)q =p
   WHERE(q>ihla)p = ihla
END SUBROUTINE sconta


SUBROUTINE riduci_grd
   USE tinclude2                                    !    Global variables
   IMPLICIT none
   REAL :: hhww(np), x0, y0, xye(2,4,nl), xm, ym
   INTEGER :: io, i, j, ii, k, l, n, m, i1, i2, jj, nn, t
   INTEGER :: tne, tns, tnv, tnss, tnp, nis(ne), njs(nsf)

   tne = COUNT(iwh>ihla)
   tns = COUNT(juh>ihla)
   tnp = tne
   tnv = 4*tnp
   ALLOCATE (k4(ne),k5(ne),i4(ne),i5(ne),j4(nsf),j5(nsf),i7(tne),nns(nsf),kss(tne),ihw(tne))
   ALLOCATE (jk4(nsf),huj(tns),dyj(tns),hwi(tne),aai(tne),ihu(tns),xysb(2,tns),xyst(2,tns))
   ALLOCATE (sxy(2,tnv))

      n = 0           ! coordinate di cell_pixel>hland
      kss = 4
      k4(1) = 1
      DO i=1,ne
         x0=xy(1,nen(4,i))
         y0=xy(2,nen(4,i))
         m=0
         DO j=0,nl-1
            xye(2,1:2,:) = y0+(j  )*dely
            xye(2,3:4,:) = y0+(j+1)*dely
            xye(1,1  ,:) = x0+(/(k,k=0,nl-1)/)*delx
            xye(1,4  ,:) = x0+(/(k,k=0,nl-1)/)*delx
            xye(1,2  ,:) = x0+(/(k,k=1,nl  )/)*delx
            xye(1,3  ,:) = x0+(/(k,k=1,nl  )/)*delx
            DO k=0,nl-1
               IF(iwh(j*nl+k+1,i)<=ihla)CYCLE
               n = n+1
               m = m+1
               ihw(n) = m
               sxy(:,4*(n-1)+1:4*n) = xye(:,:,k+1)
            END DO
         END DO
         k5(i) = n
         IF(i<ne)k4(i+1) = k5(i)+1
      END DO

      tnp = n
      tnv = tnp*4
      FORALL(i=1:tnp)i7(i) = 1+4*(i-1)

      n = 0                !      riduce la cella
      i4(1) = 1
      DO i=1,ne
         m = 0
         DO j=0,nl-1
            DO k=0,nl-1
               IF(iwh(j*nl+k+1,i)<=ihla)CYCLE
               m = m+1
               hhww(m) = iwh(j*nl+k+1,i)*scale
            END DO
         END DO
         nn = 0
         DO j=0,nl-1
            DO k=0,nl-1
               IF(iwh(j*nl+k+1,i)<=ihla)CYCLE
               m = COUNT(iwh(:,i)==iwh(j*nl+k+1,i))
               n  = n+1
               nn  = nn+1
               aai(n) = apix*m
               hwi (n) = iwh(j*nl+k+1,i)*scale
               WHERE(iwh(:,i)*scale==hwi(n)) iwh(:,i) = ihla
               WHERE(hhww(1:k5(i)-k4(i)+1)==hwi(n)) ihw(k4(i):k5(i)) = nn
            END DO
         END DO
         i5(i) = n
         IF(i<ne)i4(i+1) = i5(i)+1
      END DO
      tne = n

      n = 0           ! coordinate di edge_pixel>hland
      jk4(1) = 1
      DO j=1,nsf
         m = 0
         nns(j) = COUNT(juh(:,j)>ihla)
         DO k=1,nl
            IF(juh(k,j)<=ihla)CYCLE
            n = n+1
            m = m+1
            ihu(n) = m
            xysb(1:2,n) = xy(:,jb(j))+(k-1)*(xy(:,jt(j))-xy(:,jb(j)))/nl
            xyst(1:2,n) = xy(:,jb(j))+ k   *(xy(:,jt(j))-xy(:,jb(j)))/nl
         END DO
         IF(j<nsf)jk4(j+1) = jk4(j)+nns(j)
      END DO

      n = 0                !      riduce i lati
      j4(1) = 1
      DO j=1,nsf
         nn = 0
         DO k=1,nl
            IF(juh(k,j)<=ihla)CYCLE
            nn = nn+1
            hhww(nn) = juh(k,j)*scale
         END DO
         nn = 0
         DO k=1,nl
            IF(juh(k,j)<=ihla)CYCLE
            i = COUNT(juh(:,j)==juh(k,j))
            n=n+1
            nn=nn+1
            dyj(n) = i*MERGE(delx,dely,xy(2,jb(j))==xy(2,jt(j)))
            huj (n) = juh(k,j)*scale
            WHERE(juh(:,j)*scale==huj(n)) juh(:,j) = ihla
            WHERE(hhww(1:nns(j))==huj(n)) ihu(jk4(j):jk4(j)+nns(j)-1) = nn
         END DO
         j5(j) = n
         IF(j<nsf)j4(j+1) = j5(j)+1
      END DO
   CALL scrivi

CONTAINS
SUBROUTINE scrivi
   IMPLICIT none
   ALLOCATE (xc(2,ne))
   DO i=1,ne
	  xc(1,i)=0.5*(xy(1,nen(1,i))+xy(1,nen(4,i)))
	  xc(2,i)=0.5*(xy(2,nen(1,i))+xy(2,nen(2,i)))
   END DO

   nis = i5-i4+1
   njs = j5-j4+1
   nls = l5-l4+1
   tne = SUM(nis(1:ne ))                    ! total number of sub-polygons of the computational grid
   tns = SUM(njs(1:nsf))                    ! total number of sub-edges of the computational grid
   tnl = SUM(nls(1:nlk))                    ! total number of sub-edges of the computational grid

   Print *,'Dfrisc, stocascrev UnTRIM.grd !'
   grid_file   = 'UnTRIM.grd'
   OPEN(grd,File=grid_file)
!                                                             Parts a-b
   location=CHAR(39)//TRIM(location)//CHAR(39)
   NAMELIST /grd_2008/ nv, ne, nr, ns, nsi, nbc, nsf, tne, tns, angle, location
   WRITE(grd,NML=grd_2008)
   DO i=1,nv
      WRITE(grd,'(I10,1X,2G20.12)') i, xy(:,i)  ! vertex co-ordinates of the computational grid
   END DO
   DO i=1,ne
       WRITE(grd,'(I10,1X,I5,2(1X,G20.12),8(1X,I10))') i, 4, xc(:,i), nen(:,i),is(:,i)
   END DO
   DO j=1,ns
       WRITE(grd,'(I10,1X,4(1X,I10))') j, jb(j), jt(j), je(:,j)
   END DO 
   DO i=1,ne
       WRITE(grd,'(I10,1X,I10)') i, nis(i)
	   WRITE(grd,'(10(G20.12,1X))') aai(i4(i):i5(i))
	   WRITE(grd,'(10(G20.12,1X))') hwi(i4(i):i5(i))
   END DO
   DO j=1,nsf
       WRITE(grd,'(I10,1X,I10)') j, njs(j)
	   WRITE(grd,'(10(G20.12,1X))') dyj(j4(j):j5(j))
       WRITE(grd,'(10(G20.12,1X))') huj(j4(j):j5(j))
   END DO
   CLOSE(grd)

   OPEN(grd,File='link.grd')
   WRITE(grd,'(2I10,1X,A40)')nlk, tnl, ' nlk   tnl   i1    i2   h12   l12     cq'
   DO i=1,nlk
       WRITE(grd,'(4(I10,1X))') i, il(:,i), nls(i)
	   WRITE(grd,'(10(G20.12,1X))') l12(l4(i):l5(i))
       WRITE(grd,'(10(G20.12,1X))') h12(l4(i):l5(i))
       WRITE(grd,'(10(G20.12,1X))') cq(l4(i):l5(i))
   END DO
   CLOSE(grd)
!                                                             Part c
   PRINT *,'Nof subpolygons, subedges, sublinks =',tne, tns, tnl
   CALL crono
   PRINT *,'Dfrisc, stocascrev UnTRIM.grd-4-plt !'
   k5  = k5-k4+1
   tnp = SUM(k5(1:ne ))    ! total number of sub-polygons for postprocessing use (tnp>=tne)
   tnv = SUM(kss(1:tnp))   ! total number of vertices stored for postprocessing use (tnv>>nv)
   tnss = SUM(nns(1:nsf )) ! total number of sub-edges for postprocessing use (tnss>=tns)
   WRITE(*,'(a23,3i15)')'tne, tnss, tnv-4-plt =',tnp,tnss,tnv
   ! CALL compatta
   ! grid_file = TRIM(grid_file)//'-4-plt'
   ! OPEN(grd,file=grid_file,FORM='unformatted')
   ! WRITE(grd)tnp, tnv, tnss
   ! WRITE(grd)k5(1:ne), nns(1:nsf), kss(1:tnp), ihu(1:tnss), ihw(1:tnp)
   ! WRITE(grd)sxy(:,1:tnv), xysb(:,1:tnss), xyst(:,1:tnss)
   ! CLOSE(grd)
   DEALLOCATE(sxy,xysb,xyst,ihu,ihw,k5,nns,kss)
   CALL crono
   WRITE(*,*)'Done!'
END SUBROUTINE scrivi

SUBROUTINE compatta
   REAL, DIMENSION(2,4) :: temp
!$OMP PARALLEL DO PRIVATE(ii,j,temp) SCHEDULE(RUNTIME)
   DO i=1,ne                                ! compatta sub-polygons
	  DO ii=1,k5(i)-1
         IF(ihw(k4(i)+ii)==ihw(k4(i)+ii-1))CYCLE
         DO j=ii+1,k5(i)-1
            IF(ihw(k4(i)+j)/=ihw(k4(i)+ii-1))CYCLE
            ihw(k4(i)+j ) = ihw(k4(i)+ii  )
            ihw(k4(i)+ii) = ihw(k4(i)+ii-1)
            temp = sxy(:,i7(k4(i)+ii):i7(k4(i)+ii)+3)
            sxy(:,i7(k4(i)+ii):i7(k4(i)+ii)+3) = sxy(:,i7(k4(i)+j):i7(k4(i)+j)+3)
            sxy(:,i7(k4(i)+j):i7(k4(i)+j)+3) = temp
            EXIT
         END DO
	  END DO
   END DO
END SUBROUTINE compatta

END SUBROUTINE riduci_grd


SUBROUTINE crono
   IMPLICIT none
   REAL :: ttee, temp
   REAL, SAVE :: tempo, tstart
   LOGICAL, SAVE :: init=.true.
   INTEGER, DIMENSION(8) :: tnow
   IF(init)THEN
        init = .false.
        CALL CPU_TIME(tempo)
        CALL CPU_TIME(tempo)
        CALL DATE_AND_TIME(Values=tnow)
        tstart = tnow(7)+60*(tnow(6)+60*(tnow(5)+24*tnow(3)))+0.001*tnow(8)
   END IF
   CALL CPU_TIME(ttee)
   CALL DATE_AND_TIME(Values=tnow)
   temp = tnow(7)+60*(tnow(6)+60*(tnow(5)+24*tnow(3)))+0.001*tnow(8)
   WRITE(*,'(3(a11,g15.8))')' CPU time =',ttee-tempo,'  Total =',ttee,'  Elaps. =',temp-tstart
   tempo = ttee
END SUBROUTINE crono

