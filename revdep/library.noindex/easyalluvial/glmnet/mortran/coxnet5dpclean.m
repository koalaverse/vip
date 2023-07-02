"
c    coxnet subset derived from newGLMnet in glmnet5dpclean.m 11/3/2022 bt TH
c                          newGLMnet (5/12/14)
c    see inputs etc in original
"
subroutine coxnet(parm,no,ni,x,y,d,g,w,jd,vp,cl,ne,nx,nlam,flmin,ulam,thr,
   maxit,isd,lmu,ca,ia,nin,dev0,dev,alm,nlp,jerr);
implicit double precision(a-h,o-z);
double precision x(no,ni),y(no),d(no),g(no),w(no),vp(ni),ulam(nlam);
double precision ca(nx,nlam),dev(nlam),alm(nlam),cl(2,ni);
integer jd(*),ia(nx),nin(nlam);
%fortran
      double precision, dimension (:), allocatable :: xs,ww,vq
      integer, dimension (:), allocatable :: ju
%mortran
if maxval(vp).le.0.0 < jerr=10000; return;>
allocate(ww(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(ju(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(vq(1:ni),stat=jerr); if(jerr.ne.0) return;
if isd.gt.0 < allocate(xs(1:ni),stat=jerr); if(jerr.ne.0) return;>

call chkvars(no,ni,x,ju);
if(jd(1).gt.0) ju(jd(2:(jd(1)+1)))=0;
if maxval(ju).le.0 < jerr=7777; return;>
vq=max(0d0,vp); vq=vq*ni/sum(vq);
ww=max(0d0,w); sw=sum(ww);
if sw.le.0.0 < jerr=9999; return;> ww=ww/sw;
call cstandard(no,ni,x,ww,ju,isd,xs);
if isd.gt.0 < <j=1,ni; cl(:,j)=cl(:,j)*xs(j);>>
call coxnet1(parm,no,ni,x,y,d,g,ww,ju,vq,cl,ne,nx,nlam,flmin,ulam,thr,
         isd,maxit,lmu,ca,ia,nin,dev0,dev,alm,nlp,jerr);
if(jerr.gt.0) return; dev0=2.0*sw*dev0;
if isd.gt.0 < <k=1,lmu; nk=nin(k); ca(1:nk,k)=ca(1:nk,k)/xs(ia(1:nk));>>
deallocate(ww,ju,vq); if(isd.gt.0) deallocate(xs);
return;
end;
subroutine cstandard(no,ni,x,w,ju,isd,xs);
implicit double precision(a-h,o-z);
double precision x(no,ni),w(no),xs(ni); integer ju(ni);
<j=1,ni; if(ju(j).eq.0) next;
   xm=dot_product(w,x(:,j)); x(:,j)=x(:,j)-xm;
   if isd.gt.0 < xs(j)=sqrt(dot_product(w,x(:,j)**2)); x(:,j)=x(:,j)/xs(j);>
>
return;
end;
subroutine coxnet1(parm,no,ni,x,y,d,g,q,ju,vp,cl,ne,nx,nlam,flmin,ulam,cthri,
    isd,maxit,lmu,ao,m,kin,dev0,dev,alm,nlp,jerr);
implicit double precision(a-h,o-z);
double precision x(no,ni),y(no),q(no),d(no),g(no),vp(ni),ulam(nlam);
double precision ao(nx,nlam),dev(nlam),alm(nlam),cl(2,ni);
integer ju(ni),m(nx),kin(nlam);
%fortran
      double precision, dimension (:), allocatable :: w,dk,v,xs,wr
      double precision, dimension (:), allocatable :: a,as,f,dq
      double precision, dimension (:), allocatable :: e,uu,ga
      integer, dimension (:), allocatable :: jp,kp,mm,ixx
%mortran
call get_int_parms(sml,eps,big,mnlam,devmax,pmin,exmx,itrace);
"Start: Naras Edit"
isd = isd*1;
"End: Naras Edit"
sml=sml*100.0; devmax=devmax*0.99/0.999;
allocate(e(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(uu(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(f(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(w(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(v(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(a(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(as(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(xs(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(ga(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(ixx(1:ni),stat=jerr); if(jerr.ne.0) return;
allocate(jp(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(kp(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(dk(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(wr(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(dq(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(mm(1:ni),stat=jerr); if(jerr.ne.0) return;
call groups(no,y,d,q,nk,kp,jp,t0,jerr);
if(jerr.ne.0) go to :done:; alpha=parm;
oma=1.0-alpha; nlm=0; ixx=0; al=0.0;
dq=d*q; call died(no,nk,dq,kp,jp,dk);
a=0.0; f(1)=0.0; fmax=log(huge(f(1))*0.1);
if nonzero(no,g).ne.0 < f=g-dot_product(q,g);
   e=q*exp(sign(min(abs(f),fmax),f));
>
else < f=0.0; e=q;>
r0=risk(no,ni,nk,dq,dk,f,e,kp,jp,uu);
rr=-(dot_product(dk(1:nk),log(dk(1:nk)))+r0); dev0=rr;
<i=1,no; if y(i).lt.t0.or.q(i).le.0.0 < /w(i),wr(i)/=0.0;>>
call outer(no,nk,dq,dk,kp,jp,e,wr,w,jerr,uu);
if(jerr.ne.0) go to :done:;
"Begin: added by Naras"
alf=1.0;
"End: added by Naras"
if flmin.lt.1.0 < eqs=max(eps,flmin); alf=eqs**(1.0/(nlam-1));>
m=0; mm=0; /nlp,nin/=0; mnl=min(mnlam,nlam); as=0.0; cthr=cthri*dev0;
<j=1,ni; if(ju(j).eq.0) next; ga(j)=abs(dot_product(wr,x(:,j)));>
<ilm=1,nlam; if(itrace.ne.0) call setpb(ilm-1); al0=al;
   if flmin.ge.1.0 < al=ulam(ilm);>
   elseif ilm.gt.2 < al=al*alf;>
   elseif ilm.eq.1 < al=big;>
   else < al0=0.0;
      <j=1,ni; if(ju(j).eq.0) next; if(vp(j).gt.0.0) al0=max(al0,ga(j)/vp(j));>
      al0=al0/max(parm,1.0d-3); al=alf*al0;
   >
   sa=alpha*al; omal=oma*al; tlam=alpha*(2.0*al-al0);
   <k=1,ni; if(ixx(k).eq.1) next; if(ju(k).eq.0) next;
      if(ga(k).gt.tlam*vp(k)) ixx(k)=1;
   >
   :again:continue;
   loop < if(nlp.gt.maxit) <jerr=-ilm; return;>
      if(nin.gt.0) as(m(1:nin))=a(m(1:nin));
      call vars(no,ni,x,w,ixx,v);
      loop < nlp=nlp+1; dli=0.0;
         <j=1,ni; if(ixx(j).eq.0) next;
            u=a(j)*v(j)+dot_product(wr,x(:,j));
            if abs(u).le.vp(j)*sa < at=0.0;>
            else < at=max(cl(1,j),min(cl(2,j),sign(abs(u)-vp(j)*sa,u)/
               (v(j)+vp(j)*omal)));
            >
            if at.ne.a(j) < del=at-a(j); a(j)=at; dli=max(dli,v(j)*del**2);
               wr=wr-del*w*x(:,j); f=f+del*x(:,j);
               if mm(j).eq.0 < nin=nin+1; if(nin.gt.nx) exit;
                  mm(j)=nin; m(nin)=j;
               >
            >
         >
         if(nin.gt.nx) exit; if(dli.lt.cthr) exit;
         if nlp.gt.maxit < jerr=-ilm; return;>
         loop < nlp=nlp+1;  dli=0.0;
            <l=1,nin; j=m(l);
               u=a(j)*v(j)+dot_product(wr,x(:,j));
               if abs(u).le.vp(j)*sa < at=0.0;>
               else < at=max(cl(1,j),min(cl(2,j),sign(abs(u)-vp(j)*sa,u)/
                  (v(j)+vp(j)*omal)));
               >
               if at.ne.a(j) < del=at-a(j); a(j)=at; dli=max(dli,v(j)*del**2);
                  wr=wr-del*w*x(:,j); f=f+del*x(:,j);
               >
            >
            if(dli.lt.cthr) exit; if nlp.gt.maxit < jerr=-ilm; return;>
         >
      >
      if(nin.gt.nx) exit;
      e=q*exp(sign(min(abs(f),fmax),f));
      call outer(no,nk,dq,dk,kp,jp,e,wr,w,jerr,uu);
      if jerr.ne.0 < jerr=jerr-ilm;  go to :done:;>
      ix=0;
      <j=1,nin; k=m(j);
         if(v(k)*(a(k)-as(k))**2.lt.cthr) next; ix=1; exit;>
      if ix.eq.0 <
         <k=1,ni; if(ixx(k).eq.1) next; if(ju(k).eq.0) next;
            ga(k)=abs(dot_product(wr,x(:,k)));
            if ga(k).gt.sa*vp(k) < ixx(k)=1; ix=1;>
         >
         if(ix.eq.1) go to :again:;
         exit;
      >
   >
   if nin.gt.nx < jerr=-10000-ilm;  exit;>
   if(nin.gt.0) ao(1:nin,ilm)=a(m(1:nin)); kin(ilm)=nin;
   alm(ilm)=al; lmu=ilm;
   dev(ilm)=(risk(no,ni,nk,dq,dk,f,e,kp,jp,uu)-r0)/rr;
   if(ilm.lt.mnl) next; if(flmin.ge.1.0) next;
   me=0; <j=1,nin; if(ao(j,ilm).ne.0.0) me=me+1;> if(me.gt.ne) exit;
   if((dev(ilm)-dev(ilm-mnl+1))/dev(ilm).lt.sml) exit;
   if(dev(ilm).gt.devmax) exit;
>
g=f;
:done: deallocate(e,uu,w,dk,v,xs,f,wr,a,as,jp,kp,dq,mm,ga,ixx);
return;
end;

subroutine groups(no,y,d,q,nk,kp,jp,t0,jerr);
implicit double precision(a-h,o-z);
double precision y(no),d(no),q(no); integer jp(no),kp(*"nk");
<j=1,no; jp(j)=j;> call psort7(y,jp,1,no);
nj=0; <j=1,no; if(q(jp(j)).le.0.0) next; nj=nj+1; jp(nj)=jp(j);>
if nj.eq.0 < jerr=20000; return;>
j=1; until d(jp(j)).gt.0.0 < j=j+1;> until j.gt.nj;
if j.ge.nj-1 < jerr=30000; return;>
t0=y(jp(j)); j0=j-1;
if j0.gt.0 <
   until y(jp(j0)).lt.t0 < j0=j0-1;> until j0.eq.0;
   if j0.gt.0 < nj=nj-j0; <j=1,nj; jp(j)=jp(j+j0);>>
>
jerr=0; nk=0; yk=t0; j=2;
loop <
   until d(jp(j)).gt.0.0.and.y(jp(j)).gt.yk < j=j+1;> until j.gt.nj;
   nk=nk+1; kp(nk)=j-1; if(j.gt.nj) exit;
   if j.eq.nj < nk=nk+1; kp(nk)=nj; exit;>
   yk=y(jp(j)); j=j+1;
>
return;
end;
subroutine outer(no,nk,d,dk,kp,jp,e,wr,w,jerr,u);
implicit double precision(a-h,o-z);
double precision d(no),dk(nk),wr(no),w(no);
double precision e(no),u(no),b,c; integer kp(nk),jp(no);
call usk(no,nk,kp,jp,e,u);
b=dk(1)/u(1); c=dk(1)/u(1)**2; jerr=0;
<j=1,kp(1); i=jp(j);
   w(i)=e(i)*(b-e(i)*c); if w(i).le.0.0 < jerr=-30000; return;>
   wr(i)=d(i)-e(i)*b;
>
<k=2,nk; j1=kp(k-1)+1; j2=kp(k);
   b=b+dk(k)/u(k); c=c+dk(k)/u(k)**2;
   <j=j1,j2; i=jp(j);
      w(i)=e(i)*(b-e(i)*c); if w(i).le.0.0 < jerr=-30000; return;>
      wr(i)=d(i)-e(i)*b;
   >
>
return;
end;
subroutine vars(no,ni,x,w,ixx,v);
implicit double precision(a-h,o-z);
double precision x(no,ni),w(no),v(ni); integer ixx(ni);
<j=1,ni; if(ixx(j).gt.0) v(j)=dot_product(w,x(:,j)**2);>
return;
end;
subroutine died(no,nk,d,kp,jp,dk);
implicit double precision(a-h,o-z);
double precision d(no),dk(nk); integer kp(nk),jp(no);
dk(1)=sum(d(jp(1:kp(1))));
<k=2,nk; dk(k)=sum(d(jp((kp(k-1)+1):kp(k))));>
return;
end;
subroutine usk(no,nk,kp,jp,e,u);
implicit double precision(a-h,o-z);
double precision e(no),u(nk),h; integer kp(nk),jp(no);
h=0.0;
<k=nk,1,-1; j2=kp(k);
   j1=1; if(k.gt.1) j1=kp(k-1)+1;
   <j=j2,j1,-1; h=h+e(jp(j));>
   u(k)=h;
>
return;
end;
function risk(no,ni,nk,d,dk,f,e,kp,jp,u);
implicit double precision(a-h,o-z);
double precision d(no),dk(nk),f(no);
integer kp(nk),jp(no); double precision e(no),u(nk);
"Start: Naras Edit"
ni = ni*1;
"End: Naras Edit"
call usk(no,nk,kp,jp,e,u); u=log(u);
risk=dot_product(d,f)-dot_product(dk,u);
return;
end;
subroutine loglike(no,ni,x,y,d,g,w,nlam,a,flog,jerr);
implicit double precision(a-h,o-z);
double precision x(no,ni),y(no),d(no),g(no),w(no),a(ni,nlam),flog(nlam);
%fortran
      double precision, dimension (:), allocatable :: dk,f,xm,dq,q
      double precision, dimension (:), allocatable :: e,uu
      integer, dimension (:), allocatable :: jp,kp
%mortran
allocate(e(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(q(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(uu(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(f(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(dk(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(jp(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(kp(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(dq(1:no),stat=jerr); if(jerr.ne.0) return;
allocate(xm(1:ni),stat=jerr); if(jerr.ne.0) return;
q=max(0d0,w); sw=sum(q);
if sw.le.0.0 < jerr=9999; go to :done:;>
call groups(no,y,d,q,nk,kp,jp,t0,jerr);
if(jerr.ne.0) go to :done:; fmax=log(huge(e(1))*0.1);
dq=d*q; call died(no,nk,dq,kp,jp,dk); gm=dot_product(q,g)/sw;
<j=1,ni; xm(j)=dot_product(q,x(:,j))/sw;>
<lam=1,nlam;
   <i=1,no; f(i)=g(i)-gm+dot_product(a(:,lam),(x(i,:)-xm));
      e(i)=q(i)*exp(sign(min(abs(f(i)),fmax),f(i)));
   >
   flog(lam)=risk(no,ni,nk,dq,dk,f,e,kp,jp,uu);
>
:done: deallocate(e,uu,dk,f,jp,kp,dq);
return;
end;
subroutine chkvars(no,ni,x,ju);
implicit double precision(a-h,o-z);
double precision x(no,ni); integer ju(ni);
<j=1,ni; ju(j)=0; t=x(1,j);
   <i=2,no; if(x(i,j).eq.t) next; ju(j)=1; exit;>
>
return;
end;
function bnorm(b0,al1p,al2p,g,usq,jerr);
implicit double precision(a-h,o-z);
data thr,mxit /1.0d-10,100/;
b=b0; zsq=b**2+usq; if zsq.le.0.0 < bnorm=0.0; return;>
z=sqrt(zsq); f=b*(al1p+al2p/z)-g; jerr=0;
<it=1,mxit;  b=b-f/(al1p+al2p*usq/(z*zsq));
   zsq=b**2+usq; if zsq.le.0.0 < bnorm=0.0; return;>
   z=sqrt(zsq); f=b*(al1p+al2p/z)-g;
   if(abs(f).le.thr) exit; if b.le.0.0 < b=0.0; exit;>
>
bnorm=b; if(it.ge.mxit) jerr=90000;
return;
"Begin: Edited by Naras"
entry chg_bnorm(arg,irg); bnorm = 0.0; thr=arg; mxit=irg; return;
entry get_bnorm(arg,irg); bnorm = 0.0; arg=thr; irg=mxit; return;
"End: Edited by Naras"
end;

subroutine get_int_parms(sml,eps,big,mnlam,rsqmax,pmin,exmx,itrace);
implicit double precision(a-h,o-z);
data sml0,eps0,big0,mnlam0,rsqmax0,pmin0,exmx0,itrace0
  /1.0d-5,1.0d-6,9.9d35,5,0.999,1.0d-9,250.0,0/;
sml=sml0; eps=eps0; big=big0; mnlam=mnlam0; rsqmax=rsqmax0;
pmin=pmin0; exmx=exmx0; itrace=itrace0;
return;
entry chg_fract_dev(arg); sml0=arg; return;
entry chg_dev_max(arg); rsqmax0=arg; return;
entry chg_min_flmin(arg); eps0=arg; return;
entry chg_big(arg); big0=arg; return;
entry chg_min_lambdas(irg); mnlam0=irg; return;
entry chg_min_null_prob(arg); pmin0=arg; return;
entry chg_max_exp(arg); exmx0=arg; return;
entry chg_itrace(irg); itrace0=irg; return;
end;

subroutine get_int_parms2(epsnr,mxitnr);
implicit double precision(a-h,o-z);
data epsnr0,mxitnr0
  /1.0d-6,25/;
epsnr=epsnr0; mxitnr=mxitnr0; 
return;
entry chg_epsnr(arg); epsnr0=arg; return;
entry chg_mxitnr(irg); mxitnr0=irg; return;
end;
function nonzero(n,v);
implicit double precision(a-h,o-z);
double precision v(n);
nonzero=0; <i=1,n; if v(i).ne.0.0 < nonzero=1; return;>>
return;
end;

%fortran
      subroutine psort7(v,a,ii,jj)
      implicit double precision(a-h,o-z)
c
c     puts into a the permutation vector which sorts v into
c     increasing order. the array v is not modified.
c     only elements from ii to jj are considered.
c     arrays iu(k) and il(k) permit sorting up to 2**(k+1)-1 elements
c
c     this is a modification of cacm algorithm #347 by r. c. singleton,
c     which is a modified hoare quicksort.
c
      dimension a(jj),v(jj),iu(20),il(20)
      integer t,tt
      integer a
      double precision v
      m=1
      i=ii
      j=jj
 10   if (i.ge.j) go to 80
 20   k=i
      ij=(j+i)/2
      t=a(ij)
      vt=v(t)
      if (v(a(i)).le.vt) go to 30
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      vt=v(t)
 30   l=j
      if (v(a(j)).ge.vt) go to 50
      a(ij)=a(j)
      a(j)=t
      t=a(ij)
      vt=v(t)
      if (v(a(i)).le.vt) go to 50
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      vt=v(t)
      go to 50
 40   a(l)=a(k)
      a(k)=tt
 50   l=l-1
      if (v(a(l)).gt.vt) go to 50
      tt=a(l)
      vtt=v(tt)
 60   k=k+1
      if (v(a(k)).lt.vt) go to 60
      if (k.le.l) go to 40
      if (l-i.le.j-k) go to 70
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 90
 70   il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 90
 80   m=m-1
      if (m.eq.0) return
      i=il(m)
      j=iu(m)
 90   if (j-i.gt.10) go to 20
      if (i.eq.ii) go to 10
      i=i-1
 100  i=i+1
      if (i.eq.j) go to 80
      t=a(i+1)
      vt=v(t)
      if (v(a(i)).le.vt) go to 100
      k=i
 110  a(k+1)=a(k)
      k=k-1
      if (vt.lt.v(a(k))) go to 110
      a(k+1)=t
      go to 100
      end
%%
