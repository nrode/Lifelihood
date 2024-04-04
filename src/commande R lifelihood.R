

#Function to call Lifelihood from R#####################
#infile (0), customfile(1), GbyG(2), nbrunMcMC(3), interval(4),SE(5), saveprobevent(6), reparW(7), r(8), seed1-4 (9-12), ntr(13), nst(14), To(15), Tf(16), climbrate(17), precision(18) 

setwd("C:/Users/lenormand/Dropbox/LifeLihood/console")
list.files()

Lifelihood<-function(infile="100%mort_Pierrick211genoparinteraction.txt",customfile="custom.txt",GbyG=0,MCMC=0,interval=25,SEcal=0,saveprobevent=0,fitness=0,r=0,seed1=12,seed2=13,seed3=14,seed4=15,ntr=2,nst=2,To=50,Tf=1,climbrate=1,precision=0.001)

{
infile<-paste(getwd(),infile,sep="/")
infile<-gsub("/","\\",infile,fixed=T)
customfile<-paste(getwd(), customfile,sep="/")
customfile<-gsub("/","\\",customfile,fixed=T)

string<-paste(infile, customfile, GbyG, MCMC, interval, SEcal, saveprobevent, fitness, r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate,precision)
print(string)
system("lifelihoodC2023.exe", intern = FALSE,
       ignore.stdout = FALSE, ignore.stderr = FALSE,
       wait = TRUE, input = string, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)
}

Lifelihood(infile="100%mort_Pierrickintercept.txt",ntr=10,To=50,Tf=50,seed2=34)


