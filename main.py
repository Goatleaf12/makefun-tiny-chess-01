import json,os,urllib.request
MT=[0,100,320,330,500,900,0]
PA=b'\0\0\4\10\16\30(\0'
RB={7:1,0:2,119:4,112:8}
def V(a,r,f,c):
 if a==1:return PA[r]
 if a==6:return 20-15*r
 return(10-2*a)*c-(20,8,0,6)[a-2]
PV=[0]*1664
for i in range(768):a=(i>>7)+1;q=i&127;f=q&7;r=q>>4;c=min(f,7-f)+min(r,7-r);PV[(a+6)<<7|q]=MT[a]+V(a,r,f,c);PV[(6-a)<<7|q]=-MT[a]-V(a,7-r,f,c)
KN=(31,33,14,18,-31,-33,-14,-18);OR=(1,16,-1,-16);DG=(15,17,-15,-17);KG=OR+DG
B=[0]*128;EV=[0];KS=[0,0];ST=[];N=[0];LIM=40000
X=Exception
def E(p,q,k):EV[0]+=k*PV[(p+6)<<7|q]
def A(q,w):
 for t in(q-15*w,q-17*w):
  if not t&136 and B[t]==w:return 1
 for i,d in enumerate(KN+KG):
  t=q+d
  if not t&136 and B[t]==w*(2,6)[i>7]:return 1
 for i,d in enumerate(KG):
  t=q+d
  while not t&136:
   p=B[t]
   if p:
    if p*w>0 and abs(p)in(5,4-(i>3)):return 1
    break
   t+=d
 return 0
def G(cp=0):
 m=[];P=m.append;d=16*TN
 for f in range(128):
  p=B[f]
  if p*TN<=0:continue
  a=abs(p)
  if a==1:
   t=f+d
   if not B[t]:
    if(t>>4)==7*(TN>0):[P(f|t<<7|k<<14)for k in(5,4,3,2)]
    elif not cp:
     P(f|t<<7)
     if(f>>4)==6-5*(TN>0) and not B[f+2*d]:P(f|f+2*d<<7)
   for t in(f+d-1,f+d+1):
    if t&136:continue
    q=B[t]
    if q*TN<0:
     if(t>>4)==7*(TN>0):[P(f|t<<7|k<<14|abs(q)<<17)for k in(5,4,3,2)]
     else:P(f|t<<7|abs(q)<<17)
    elif t==EP:P(f|t<<7|1<<17|1<<20)
  elif a in(2,6):
   for o in(KN,KG)[a==6]:
    t=f+o
    if t&136:continue
    q=B[t]
    if not(q or cp):P(f|t<<7)
    elif q*TN<0:P(f|t<<7|abs(q)<<17)
   if a==6 and not cp:
    e=-TN
    for sg in(1,-1):
     if CR&1<<(sg<0)+2*(TN<0)and not(B[f+sg]or B[f+2*sg]or B[f-3]and sg<0)and not any(A(f+i*sg,e)for i in(0,1,2)):P(f|f+2*sg<<7)
  else:
   for o in(DG,OR,KG)[a-3]:
    t=f+o
    while not t&136:
     q=B[t]
     if q:
      q*TN<0 and P(f|t<<7|abs(q)<<17);break
     if not cp:P(f|t<<7)
     t+=o
 return m
def mk(m,u=0):
 global TN,CR,EP
 if u:m,oc,oe,p=ST.pop();s=-TN;n=B[(m>>7)&127]
 else:p=B[m&127];s=TN;ST.append((m,CR,EP,p));n=((m>>14)&7 or abs(p))*s
 f=m&127;t=(m>>7)&127;cp=(m>>17)&7;cs=t-16*s*(m>>20&1)
 if cp and not u:E(B[cs],cs,-1);B[cs]=0
 x,y,z,w=(n,t,p,f)if u else(p,f,n,t)
 E(x,y,-1);B[w]=z;B[y]=0;E(z,w,1)
 if cp and u:q=-s*cp;B[cs]=q;E(q,cs,1)
 if abs(p)==6:
  KS[s<0]=f if u else t;rf=-1
  if t-f==2:rf,rt=t+1,t-1
  elif f-t==2:rf,rt=t-2,t+1
  if rf>=0:
   x,y=(rf,rt)[::1-2*u];E(s*4,x,-1);B[x],B[y]=0,s*4;E(s*4,y,1)
  if not u:CR&=(-4,-13)[s<0]
 if u:CR=oc;EP=oe
 else:
  CR&=~RB.get(f,0)&~RB.get(t,0)
  EP=(f+t)//2 if abs(p)==1 and abs(t-f)==32 else-1
 TN=-TN
def parse(f):
 global TN,CR,EP
 w=f.split();B[:]=[0]*128;s=112;EV[0]=0
 for c in w[0]:
  if c>'9':
   v='PNBRQKpnbrqk'.index(c)%6+1;B[s]=v if c<'a' else-v;EV[0]+=PV[(B[s]+6)<<7|s]
   if v==6:KS[B[s]<0]=s
   s+=1
  else:s+=int(c)if c!='/'else-24
 TN=1 if w[1]=='w'else-1
 CR=sum(1<<'KQkq'.find(c)for c in w[2]if c>'-')
 EP=ord(w[3][0])+16*ord(w[3][1])-881 if w[3]!='-'else-1
def S(d,a,b):
 N[0]+=1
 if N[0]>LIM:raise X
 if d<-32:return EV[0]*TN
 ck=A(KS[TN<0],-TN)
 if d<=0 and not ck:
  s=EV[0]*TN
  if s>=b:return s
  if s>a:a=s
 ms=G() if d>0 or ck else G(1)
 ms.sort(key=lambda m:-(m>>17&7))
 nl=0
 for m in ms:
  if d<=0 and not ck and MT[m>>17&7]+100<MT[abs(B[m&127])]:continue
  mk(m)
  if A(KS[TN>0],TN):mk(0,1);continue
  nl+=1;v=-S(d-1,-b,-a);mk(0,1)
  if v>a:a=v
  if a>=b:break
 if not nl:return-100000 if ck else 0 if d>0 else a
 return a
def root(f):
 parse(f)
 ms=G()
 N[0]=0;bs=ms[0]
 try:
  for d in range(1,9):
   ms.sort(key=bs.__ne__)
   cb=bs;bv=-10**9
   for m in ms:
    mk(m)
    if A(KS[TN>0],TN):mk(0,1);continue
    v=-S(d-1,-10**9,-bv);mk(0,1)
    if v>bv:bv=v;cb=m
   bs=cb
   if bv>90000:break
 except X:0
 f=bs&127;t=bs>>7&127;p=bs>>14&7
 u=bytes((97+f%8,49+f//16,97+t%8,49+t//16)).decode()+('  nbrq'[p]if p else'')
 return u
def post(p,b):return json.load(urllib.request.urlopen(os.getenv('MAKEFUN_ENV_URL')+p,json.dumps(b).encode()))
g={}
while g.get('status')!='done':
 g=post('/game',{})
 while g['status']=='your_move':g=post('/move',{'game':g['game'],'move':root(g['fen'])})
