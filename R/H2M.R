H2M=function(BASE,lang="FR",type="ts"){
if(lang=="FR"){
	X=BASE[,2]
	Y=BASE[,1]
	date1=substr(as.character(Y),1,10)
	date2=substr(as.character(Y),14,23)
	D1=as.Date(date1,"%Y-%m-%d")
	D2=as.Date(date2,"%Y-%m-%d")
	vm=vy=N=NA
	for(t in 1:length(D1)){
	mois=seq(D1[t],D2[t],length=7)
	vm=c(vm,as.POSIXlt(mois)$mon+1)
	vy=c(vy,as.POSIXlt(mois)$year+1900)
	N=c(N,rep(X[t],7))}
	N=N[-1]; vm=vm[-1]; vy=vy[-1]
	YM=vy*100+vm
	Z=tapply(N,as.factor(YM),mean)}
if(lang=="ENG"){
	Mois = c("Jan","Fév", "Mar", "Avr",
	"Mai", "Jui", "Jul", "Aoû", "Sep", "Oct",
	"Nov", "Déc")
	Months = c("Jan", "Feb", "Mar", "Apr",
	"May", "Jun", "Jul", "Aug",
	"Sep", "Oct", "Nov", "Dec")
	X=BASE[,2]
	jour=BASE[,1]
	D1=as.Date(as.character(jour),"%b %d %Y")
	if(sum(is.na(D1)>0)){
	moisGB=substr(as.character(jour),1,3)
	V=1:12; names(V)=Months
	NoMois=as.numeric(V[moisGB])
	moisFR=Mois[NoMois]
	jourFR=paste(moisFR,substr(as.character(
	jour),4,nchar(as.character(jour))),sep="")
	D1=as.Date(jourFR,"%b %d %Y")
	}
	D2=D1+6
	vm=vy=N=NA
	for(t in 1:length(D1)){
	mois=seq(D1[t],D2[t],length=7)
	vm=c(vm,as.POSIXlt(mois)$mon+1)
	vy=c(vy,as.POSIXlt(mois)$year+1900)
	N=c(N,rep(X[t],7))}
	N=N[-1]; vm=vm[-1]; vy=vy[-1]
	YM=vy*100+vm
	Z=tapply(N,as.factor(YM),mean)}
if(type=="ts"){
	Zts=ts(as.numeric(Z),start=c(2004,1),frequency=12)}
if(type=="numeric"){
	Zts=as.numeric(Z)}
return(Zts)}
