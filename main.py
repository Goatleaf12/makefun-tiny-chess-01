import json,os,urllib.request as rq,random
MV=[0,100,320,330,500,900,0]
RK={7:1,0:2,119:4,112:8}
def VL(a,rq,f,c):
 return(0,0,4,8,16,30,40,0)[rq]if a==1 else 20-15*rq if a==6 else(6,4,2,2)[a-3]*c-(20,8,0,6)[a-2]
PS=[0]*1664
for i in range(768):a=(i>>7)+1;q=i&127;f=q&7;k=q>>4;c=min(f,7-f)+min(k,7-k);PS[(a+6)<<7|q]=MV[a]+VL(a,k,f,c);PS[(6-a)<<7|q]=-MV[a]-VL(a,7-k,f,c)
KT=(31,33,14,18,-31,-33,-14,-18);OG=(1,16,-1,-16);DB=(15,17,-15,-17);KF=OG+DB
D=[0]*128;ES=[0];KP=[0,0];SK=[];NC=[0];LM=60000;HR=[]
ZB=[*map(random.Random(1).getrandbits,[64]*1664)]
J=[0];Q={};K=[0]*64;BI=[0]
EX=Exception
def EF(p,q,k):
 i=(p+6)<<7|q;ES[0]+=k*PS[i];J[0]^=ZB[i]
def AT(q,w):
 for i,d in enumerate(KT+KF):
  t=q+d
  if not t&136 and(D[t]==w*(2,6)[i>7]or D[t]==w and i>11 and(i<14)==(w<0)):return 1
 for i,d in enumerate(KF):
  t=q+d
  while not t&136:
   p=D[t]
   if p:
    if p*w>0 and abs(p)in(5,4-(i>3)):return 1
    break
   t+=d
 return 0
def M(cp=0):
 m=[];P=m.append;d=16*W
 for f in range(128):
  p=D[f]
  if p*W<=0:continue
  a=abs(p)
  if a==1:
   for o in(0,-1,1):
    t=f+d+o
    if t&136:continue
    q=D[t]
    if o:
     if q*W<0:e=abs(q)<<17
     elif t==Y:e=1<<17|1<<20
     else:continue
    elif q or cp:continue
    else:e=0
    if t>>4==7*(W>0):[P(f|t<<7|k<<14|e)for k in(5,4,3,2)]
    elif o:P(f|t<<7|e)
    else:
     P(f|t<<7)
     if f>>4==6-5*(W>0) and not D[f+2*d]:P(f|f+2*d<<7)
  else:
   for o in(KT,DB,OG,KF,KF)[a-2]:
    t=f+o
    while not t&136:
     q=D[t]
     if q:
      q*W<0 and P(f|t<<7|abs(q)<<17);break
     if not cp:P(f|t<<7)
     if a in(2,6):break
     t+=o
   if a==6 and not cp:
    e=-W
    for sg in(1,-1):
     if C&1<<(sg<0)+2*(W<0)and not(D[f+sg]or D[f+2*sg]or D[f-3]and sg<0)and not any(AT(f+i*sg,e)for i in(0,1,2)):P(f|f+2*sg<<7)
 return m
def U(m,u=0):
 global W,C,Y
 if u:m,oc,oe,p=SK.pop();s=-W;n=D[(m>>7)&127]
 else:p=D[m&127];s=W;SK.append((m,C,Y,p));n=((m>>14)&7 or abs(p))*s
 f=m&127;t=(m>>7)&127;cp=(m>>17)&7;cs=t-16*s*(m>>20&1)
 if cp and not u:EF(D[cs],cs,-1);D[cs]=0
 x,y,z,w=(n,t,p,f)if u else(p,f,n,t)
 EF(x,y,-1);D[w]=z;D[y]=0;EF(z,w,1)
 if cp and u:q=-s*cp;D[cs]=q;EF(q,cs,1)
 if abs(p)==6:
  KP[s<0]=f if u else t
  if abs(t-f)==2:
   d=1 if t>f else-1;x,y=(t+d+d*(d<0),t-d)[::1-2*u];EF(s*4,x,-1);D[x],D[y]=0,s*4;EF(s*4,y,1)
  if not u:C&=(-4,-13)[s<0]
 if u:C=oc;Y=oe
 else:
  C&=~RK.get(f,0)&~RK.get(t,0)
  Y=(f+t)//2 if p*p==1 and abs(t-f)==32 else-1
 W=-W;J[0]^=1
def PR(f):
 global W,C,Y
 w=f.split();D[:]=[0]*128;s=112;ES[0]=0;J[0]=0
 for c in w[0]:
  if c>'9':
   v='PNBRQKpnbrqk'.index(c)%6+1;D[s]=v if c<'a' else-v;EF(D[s],s,1)
   if v==6:KP[D[s]<0]=s
   s+=1
  else:s+=int(c)if c!='/'else-24
 W=1 if w[1]=='w'else-1;J[0]^=W<0
 C=sum(1<<'KQkq'.find(c)for c in w[2]if c>'-')
 Y=ord(w[3][0])+16*ord(w[3][1])-881 if w[3]!='-'else-1
def F(d,a,b,r=0):
 NC[0]+=1
 if NC[0]>LM:raise EX
 if not r and HR.count(J[0])>1:return-10
 if d<-32:return ES[0]*W+8
 e=Q.get(J[0])if d>0 else 0;tm=e[3]if e else 0
 if e and e[0]>=d and(e[1]==0 or(e[2]>=b if e[1]==1 else e[2]<=a)):return e[2]
 ck=AT(KP[W<0],-W)
 if d<=0 and not ck:
  a=max(a,ES[0]*W+8)
  if a>=b:return a
 ms=M() if d>0 or ck else M(1)
 ms.sort(key=lambda m:-(m>>14)-(m==tm)*90-(m==K[d])*30)
 nl=0;bm=0;a0=a
 for m in ms:
  if d<=0 and not ck and MV[m>>17&7]<MV[abs(D[m&127])]:continue
  U(m)
  if AT(KP[W>0],W):U(0,1);continue
  nl+=1
  v=-F(d-1,-b,-a)
  U(0,1)
  if v>a:a=v;bm=m
  if a>=b:
   if d>0 and m<16384:K[d]=m
   break
 if not nl:return-100000 if ck else 0 if d>0 else a
 if r:BI[0]=bm
 if d>0:Q[J[0]]=(d,(a>=b)-(a<=a0),a,bm)
 return a
def RT(f):
 PR(f)
 if f[-4:]==' 0 1':HR.clear()
 HR.append(J[0])
 Q.clear()
 ms=M();NC[0]=0;BI[0]=ms[0]
 try:
  for d in range(1,9):
   F(d,-10**9,10**9,1)
 except EX:0
 bs=BI[0]
 f=bs&127;t=bs>>7&127;p=bs>>14&7
 u=''.join(chr(97+(x&7))+str(1+x//16)for x in(f,t))+('  nbrq'[p]if p else'')
 return u
def PT(p,b):return json.load(rq.urlopen(os.getenv('MAKEFUN_ENV_URL')+p,json.dumps(b).encode()))
g={}
while g.get('status')!='done':
 g=PT('/game',{})
 while g['status']=='your_move':g=PT('/move',{'game':g['game'],'move':RT(g['fen'])})
